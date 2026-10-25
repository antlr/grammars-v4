/******************************************************************************
 *
 *  Copyright 2003-2016 Broadcom Corporation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at:
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 ******************************************************************************/

/******************************************************************************
 *
 *  Interface to AVRCP mandatory commands
 *
 ******************************************************************************/
#include <base/logging.h>
#include <string.h>

#include <log/log.h>

#include "avrc_api.h"
#include "avrc_int.h"
#include "bt_common.h"
#include "btu.h"
#include "osi/include/fixed_queue.h"
#include "osi/include/osi.h"

/*****************************************************************************
 *  Global data
 ****************************************************************************/

#define AVRC_MAX_RCV_CTRL_EVT AVCT_BROWSE_UNCONG_IND_EVT

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

static const uint8_t avrc_ctrl_event_map[] = {
    AVRC_OPEN_IND_EVT,         /* AVCT_CONNECT_CFM_EVT */
    AVRC_OPEN_IND_EVT,         /* AVCT_CONNECT_IND_EVT */
    AVRC_CLOSE_IND_EVT,        /* AVCT_DISCONNECT_CFM_EVT */
    AVRC_CLOSE_IND_EVT,        /* AVCT_DISCONNECT_IND_EVT */
    AVRC_CONG_IND_EVT,         /* AVCT_CONG_IND_EVT */
    AVRC_UNCONG_IND_EVT,       /* AVCT_UNCONG_IND_EVT */
    AVRC_BROWSE_OPEN_IND_EVT,  /* AVCT_BROWSE_CONN_CFM_EVT   */
    AVRC_BROWSE_OPEN_IND_EVT,  /* AVCT_BROWSE_CONN_IND_EVT   */
    AVRC_BROWSE_CLOSE_IND_EVT, /* AVCT_BROWSE_DISCONN_CFM_EVT */
    AVRC_BROWSE_CLOSE_IND_EVT, /* AVCT_BROWSE_DISCONN_IND_EVT */
    AVRC_BROWSE_CONG_IND_EVT,  /* AVCT_BROWSE_CONG_IND_EVT    */
    AVRC_BROWSE_UNCONG_IND_EVT /* AVCT_BROWSE_UNCONG_IND_EVT  */
};

/* use this unused opcode to indication no need to call the callback function */
#define AVRC_OP_DROP 0xFE
/* use this unused opcode to indication no need to call the callback function &
 * free buffer */
#define AVRC_OP_DROP_N_FREE 0xFD

#define AVRC_OP_UNIT_INFO_RSP_LEN 8
#define AVRC_OP_SUB_UNIT_INFO_RSP_LEN 8
#define AVRC_OP_REJ_MSG_LEN 11

/* Flags definitions for AVRC_MsgReq */
#define AVRC_MSG_MASK_IS_VENDOR_CMD 0x01
#define AVRC_MSG_MASK_IS_CONTINUATION_RSP 0x02

/******************************************************************************
 *
 * Function         avrc_ctrl_cback
 *
 * Description      This is the callback function used by AVCTP to report
 *                  received link events.
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
static void avrc_ctrl_cback(uint8_t handle, uint8_t event, uint16_t result,
                            const RawAddress* peer_addr) {
  uint8_t avrc_event;

  if (event <= AVRC_MAX_RCV_CTRL_EVT && avrc_cb.ccb[handle].ctrl_cback) {
    avrc_event = avrc_ctrl_event_map[event];
    if (event == AVCT_CONNECT_CFM_EVT) {
      if (result != 0) /* failed */
        avrc_event = AVRC_CLOSE_IND_EVT;
    }
    avrc_cb.ccb[handle].ctrl_cback.Run(handle, avrc_event, result, peer_addr);
  }

  if ((event == AVCT_DISCONNECT_CFM_EVT) ||
      (event == AVCT_DISCONNECT_IND_EVT)) {
    avrc_flush_cmd_q(handle);
    alarm_free(avrc_cb.ccb_int[handle].tle);
    avrc_cb.ccb_int[handle].tle = NULL;
  }
}

/******************************************************************************
 *
 * Function         avrc_flush_cmd_q
 *
 * Description      Flush command queue for the specified avrc handle
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
void avrc_flush_cmd_q(uint8_t handle) {
  AVRC_TRACE_DEBUG("AVRC: Flushing command queue for handle=0x%02x", handle);
  avrc_cb.ccb_int[handle].flags &= ~AVRC_CB_FLAGS_RSP_PENDING;

  alarm_cancel(avrc_cb.ccb_int[handle].tle);
  fixed_queue_free(avrc_cb.ccb_int[handle].cmd_q, osi_free);
  avrc_cb.ccb_int[handle].cmd_q = NULL;
}

/******************************************************************************
 *
 * Function         avrc_process_timeout
 *
 * Description      Handle avrc command timeout
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
void avrc_process_timeout(void* data) {
  tAVRC_PARAM* param = (tAVRC_PARAM*)data;

  AVRC_TRACE_DEBUG("AVRC: command timeout (handle=0x%02x, label=0x%02x)",
                   param->handle, param->label);

  /* Notify app */
  if (avrc_cb.ccb[param->handle].ctrl_cback) {
    avrc_cb.ccb[param->handle].ctrl_cback.Run(
        param->handle, AVRC_CMD_TIMEOUT_EVT, param->label, NULL);
  }

  /* If vendor command timed-out, then send next command in the queue */
  if (param->msg_mask & AVRC_MSG_MASK_IS_VENDOR_CMD) {
    avrc_send_next_vendor_cmd(param->handle);
  }
  osi_free(param);
}

/******************************************************************************
 *
 * Function         avrc_send_next_vendor_cmd
 *
 * Description      Dequeue and send next vendor command for given handle
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
void avrc_send_next_vendor_cmd(uint8_t handle) {
  BT_HDR* p_next_cmd;
  uint8_t next_label;

  while ((p_next_cmd = (BT_HDR*)fixed_queue_try_dequeue(
              avrc_cb.ccb_int[handle].cmd_q)) != NULL) {
    p_next_cmd->event &= 0xFF;                      /* opcode */
    next_label = (p_next_cmd->layer_specific) >> 8; /* extract label */
    p_next_cmd->layer_specific &= 0xFF; /* AVCT_DATA_CTRL or AVCT_DATA_BROWSE */

    AVRC_TRACE_DEBUG(
        "AVRC: Dequeuing command 0x%08x (handle=0x%02x, label=0x%02x)",
        p_next_cmd, handle, next_label);

    /* Send the message */
    if ((AVCT_MsgReq(handle, next_label, AVCT_CMD, p_next_cmd)) ==
        AVCT_SUCCESS) {
      /* Start command timer to wait for response */
      avrc_start_cmd_timer(handle, next_label, AVRC_MSG_MASK_IS_VENDOR_CMD);
      return;
    }
  }

  if (p_next_cmd == NULL) {
    /* cmd queue empty */
    avrc_cb.ccb_int[handle].flags &= ~AVRC_CB_FLAGS_RSP_PENDING;
  }
}

/******************************************************************************
 *
 * Function         avrc_start_cmd_timer
 *
 * Description      Start timer for waiting for responses
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
void avrc_start_cmd_timer(uint8_t handle, uint8_t label, uint8_t msg_mask) {
  tAVRC_PARAM* param =
      static_cast<tAVRC_PARAM*>(osi_malloc(sizeof(tAVRC_PARAM)));
  param->handle = handle;
  param->label = label;
  param->msg_mask = msg_mask;

  AVRC_TRACE_DEBUG("AVRC: starting timer (handle=0x%02x, label=0x%02x)", handle,
                   label);

  alarm_set_on_mloop(avrc_cb.ccb_int[handle].tle, AVRC_CMD_TOUT_MS,
                     avrc_process_timeout, param);
}

/******************************************************************************
 *
 * Function         avrc_get_data_ptr
 *
 * Description      Gets a pointer to the data payload in the packet.
 *
 * Returns          A pointer to the data payload.
 *
 *****************************************************************************/
static uint8_t* avrc_get_data_ptr(BT_HDR* p_pkt) {
  return (uint8_t*)(p_pkt + 1) + p_pkt->offset;
}

/******************************************************************************
 *
 * Function         avrc_copy_packet
 *
 * Description      Copies an AVRC packet to a new buffer. In the new buffer,
 *                  the payload offset is at least AVCT_MSG_OFFSET octets.
 *
 * Returns          The buffer with the copied data.
 *
 *****************************************************************************/
static BT_HDR* avrc_copy_packet(BT_HDR* p_pkt, int rsp_pkt_len) {
  const int offset = MAX(AVCT_MSG_OFFSET, p_pkt->offset);
  const int pkt_len = MAX(rsp_pkt_len, p_pkt->len);
  BT_HDR* p_pkt_copy = (BT_HDR*)osi_malloc(BT_HDR_SIZE + offset + pkt_len);

  /* Copy the packet header, set the new offset, and copy the payload */
  memcpy(p_pkt_copy, p_pkt, BT_HDR_SIZE);
  p_pkt_copy->offset = offset;
  uint8_t* p_data = avrc_get_data_ptr(p_pkt);
  uint8_t* p_data_copy = avrc_get_data_ptr(p_pkt_copy);
  memcpy(p_data_copy, p_data, p_pkt->len);

  return p_pkt_copy;
}

/******************************************************************************
 *
 * Function         avrc_prep_end_frag
 *
 * Description      This function prepares an end response fragment
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
static void avrc_prep_end_frag(uint8_t handle) {
  tAVRC_FRAG_CB* p_fcb;
  BT_HDR* p_pkt_new;
  uint8_t *p_data, *p_orig_data;
  uint8_t rsp_type;

  AVRC_TRACE_DEBUG("%s", __func__);
  p_fcb = &avrc_cb.fcb[handle];

  /* The response type of the end fragment should be the same as the the PDU of
   * "End Fragment Response" Errata:
   * https://www.bluetooth.org/errata/errata_view.cfm?errata_id=4383
   */
  p_orig_data = ((uint8_t*)(p_fcb->p_fmsg + 1) + p_fcb->p_fmsg->offset);
  rsp_type = ((*p_orig_data) & AVRC_CTYPE_MASK);

  p_pkt_new = p_fcb->p_fmsg;
  p_pkt_new->len -=
      (AVRC_MAX_CTRL_DATA_LEN - AVRC_VENDOR_HDR_SIZE - AVRC_MIN_META_HDR_SIZE);
  p_pkt_new->offset +=
      (AVRC_MAX_CTRL_DATA_LEN - AVRC_VENDOR_HDR_SIZE - AVRC_MIN_META_HDR_SIZE);
  p_data = (uint8_t*)(p_pkt_new + 1) + p_pkt_new->offset;
  *p_data++ = rsp_type;
  *p_data++ = (AVRC_SUB_PANEL << AVRC_SUBTYPE_SHIFT);
  *p_data++ = AVRC_OP_VENDOR;
  AVRC_CO_ID_TO_BE_STREAM(p_data, AVRC_CO_METADATA);
  *p_data++ = p_fcb->frag_pdu;
  *p_data++ = AVRC_PKT_END;

  /* 4=pdu, pkt_type & len */
  UINT16_TO_BE_STREAM(
      p_data, (p_pkt_new->len - AVRC_VENDOR_HDR_SIZE - AVRC_MIN_META_HDR_SIZE));
}

/******************************************************************************
 *
 * Function         avrc_send_continue_frag
 *
 * Description      This function sends a continue response fragment
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_BAD_HANDLE if handle is invalid.
 *
 *****************************************************************************/
static uint16_t avrc_send_continue_frag(uint8_t handle, uint8_t label) {
  tAVRC_FRAG_CB* p_fcb;
  BT_HDR *p_pkt_old, *p_pkt;
  uint8_t *p_old, *p_data;
  uint8_t cr = AVCT_RSP;

  p_fcb = &avrc_cb.fcb[handle];
  p_pkt = p_fcb->p_fmsg;

  AVRC_TRACE_DEBUG("%s handle = %u label = %u len = %d", __func__, handle,
                   label, p_pkt->len);
  if (p_pkt->len > AVRC_MAX_CTRL_DATA_LEN) {
    int offset_len = MAX(AVCT_MSG_OFFSET, p_pkt->offset);
    p_pkt_old = p_fcb->p_fmsg;
    p_pkt = (BT_HDR*)osi_malloc(AVRC_PACKET_LEN + offset_len + BT_HDR_SIZE);
    p_pkt->len = AVRC_MAX_CTRL_DATA_LEN;
    p_pkt->offset = AVCT_MSG_OFFSET;
    p_pkt->layer_specific = p_pkt_old->layer_specific;
    p_pkt->event = p_pkt_old->event;
    p_old = (uint8_t*)(p_pkt_old + 1) + p_pkt_old->offset;
    p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
    memcpy(p_data, p_old, AVRC_MAX_CTRL_DATA_LEN);
    /* use AVRC continue packet type */
    p_data += AVRC_VENDOR_HDR_SIZE;
    p_data++; /* pdu */
    *p_data++ = AVRC_PKT_CONTINUE;
    /* 4=pdu, pkt_type & len */
    UINT16_TO_BE_STREAM(p_data,
                        (AVRC_MAX_CTRL_DATA_LEN - AVRC_VENDOR_HDR_SIZE - 4));

    /* prepare the left over for as an end fragment */
    avrc_prep_end_frag(handle);
  } else {
    /* end fragment. clean the control block */
    p_fcb->frag_enabled = false;
    p_fcb->p_fmsg = NULL;
  }
  return AVCT_MsgReq(handle, label, cr, p_pkt);
}

/******************************************************************************
 *
 * Function         avrc_proc_vendor_command
 *
 * Description      This function processes received vendor command.
 *
 * Returns          if not NULL, the response to send right away.
 *
 *****************************************************************************/
static BT_HDR* avrc_proc_vendor_command(uint8_t handle, uint8_t label,
                                        BT_HDR* p_pkt,
                                        tAVRC_MSG_VENDOR* p_msg) {
  BT_HDR* p_rsp = NULL;
  uint8_t* p_data;
  uint8_t* p_begin;
  uint8_t pkt_type;
  bool abort_frag = false;
  tAVRC_STS status = AVRC_STS_NO_ERROR;
  tAVRC_FRAG_CB* p_fcb;

  p_begin = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
  p_data = p_begin + AVRC_VENDOR_HDR_SIZE;
  pkt_type = *(p_data + 1) & AVRC_PKT_TYPE_MASK;

  if (pkt_type != AVRC_PKT_SINGLE) {
    /* reject - commands can only be in single packets at AVRCP level */
    AVRC_TRACE_ERROR("commands must be in single packet pdu:0x%x", *p_data);
    /* use the current GKI buffer to send the reject */
    status = AVRC_STS_BAD_CMD;
  }
  /* check if there are fragments waiting to be sent */
  else if (avrc_cb.fcb[handle].frag_enabled) {
    p_fcb = &avrc_cb.fcb[handle];
    if (p_msg->company_id == AVRC_CO_METADATA) {
      switch (*p_data) {
        case AVRC_PDU_ABORT_CONTINUATION_RSP:
          /* aborted by CT - send accept response */
          abort_frag = true;
          p_begin = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
          *p_begin = (AVRC_RSP_ACCEPT & AVRC_CTYPE_MASK);
          if (*(p_data + 4) != p_fcb->frag_pdu) {
            *p_begin = (AVRC_RSP_REJ & AVRC_CTYPE_MASK);
            *(p_data + 4) = AVRC_STS_BAD_PARAM;
          } else {
            p_data = (p_begin + AVRC_VENDOR_HDR_SIZE + 2);
            UINT16_TO_BE_STREAM(p_data, 0);
            p_pkt->len = (p_data - p_begin);
          }
          AVCT_MsgReq(handle, label, AVCT_RSP, p_pkt);
          p_msg->hdr.opcode =
              AVRC_OP_DROP; /* used the p_pkt to send response */
          break;

        case AVRC_PDU_REQUEST_CONTINUATION_RSP:
          if (*(p_data + 4) == p_fcb->frag_pdu) {
            avrc_send_continue_frag(handle, label);
            p_msg->hdr.opcode = AVRC_OP_DROP_N_FREE;
          } else {
            /* the pdu id does not match - reject the command using the current
             * GKI buffer */
            AVRC_TRACE_ERROR(
                "%s continue pdu: 0x%x does not match the current pdu: 0x%x",
                __func__, *(p_data + 4), p_fcb->frag_pdu);
            status = AVRC_STS_BAD_PARAM;
            abort_frag = true;
          }
          break;

        default:
          /* implicit abort */
          abort_frag = true;
      }
    } else {
      abort_frag = true;
      /* implicit abort */
    }

    if (abort_frag) {
      osi_free_and_reset((void**)&p_fcb->p_fmsg);
      p_fcb->frag_enabled = false;
    }
  }

  if (status != AVRC_STS_NO_ERROR) {
    p_rsp = (BT_HDR*)osi_malloc(BT_DEFAULT_BUFFER_SIZE);
    p_rsp->offset = p_pkt->offset;
    p_data = (uint8_t*)(p_rsp + 1) + p_pkt->offset;
    *p_data++ = AVRC_RSP_REJ;
    p_data += AVRC_VENDOR_HDR_SIZE; /* pdu */
    *p_data++ = 0;                  /* pkt_type */
    UINT16_TO_BE_STREAM(p_data, 1); /* len */
    *p_data++ = status;             /* error code */
    p_rsp->len = AVRC_VENDOR_HDR_SIZE + 5;
  }

  return p_rsp;
}

/******************************************************************************
 *
 * Function         avrc_proc_far_msg
 *
 * Description      This function processes metadata fragmenation
 *                  and reassembly
 *
 * Returns          0, to report the message with msg_cback .
 *
 *****************************************************************************/
static uint8_t avrc_proc_far_msg(uint8_t handle, uint8_t label, uint8_t cr,
                                 BT_HDR** pp_pkt, tAVRC_MSG_VENDOR* p_msg) {
  BT_HDR* p_pkt = *pp_pkt;
  uint8_t* p_data;
  uint8_t drop_code = 0;
  bool buf_overflow = false;
  BT_HDR* p_rsp = NULL;
  BT_HDR* p_cmd = NULL;
  bool req_continue = false;
  BT_HDR* p_pkt_new = NULL;
  uint8_t pkt_type;
  tAVRC_RASM_CB* p_rcb;
  tAVRC_NEXT_CMD avrc_cmd;
  tAVRC_STS status;

  p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;

  /* Skip over vendor header (ctype, subunit*, opcode, CO_ID) */
  p_data += AVRC_VENDOR_HDR_SIZE;

  pkt_type = *(p_data + 1) & AVRC_PKT_TYPE_MASK;
  AVRC_TRACE_DEBUG("pkt_type %d", pkt_type);
  p_rcb = &avrc_cb.rcb[handle];

  /* check if the message needs to be re-assembled */
  if (pkt_type == AVRC_PKT_SINGLE || pkt_type == AVRC_PKT_START) {
    /* previous fragments need to be dropped, when received another new message
     */
    p_rcb->rasm_offset = 0;
    osi_free_and_reset((void**)&p_rcb->p_rmsg);
  }

  if (pkt_type != AVRC_PKT_SINGLE && cr == AVCT_RSP) {
    /* not a single response packet - need to re-assemble metadata messages */
    if (pkt_type == AVRC_PKT_START) {
      /* Allocate buffer for re-assembly */
      p_rcb->rasm_pdu = *p_data;
      p_rcb->p_rmsg = (BT_HDR*)osi_malloc(BT_DEFAULT_BUFFER_SIZE);
      /* Copy START packet to buffer for re-assembling fragments */
      memcpy(p_rcb->p_rmsg, p_pkt, sizeof(BT_HDR)); /* Copy bt hdr */

      /* Copy metadata message */
      memcpy((uint8_t*)(p_rcb->p_rmsg + 1),
             (uint8_t*)(p_pkt + 1) + p_pkt->offset, p_pkt->len);

      /* offset of start of metadata response in reassembly buffer */
      p_rcb->p_rmsg->offset = p_rcb->rasm_offset = 0;

      /*
       * Free original START packet, replace with pointer to
       * reassembly buffer.
       */
      osi_free(p_pkt);
      *pp_pkt = p_rcb->p_rmsg;

      /*
       * Set offset to point to where to copy next - use the same
       * reassembly logic as AVCT.
       */
      p_rcb->p_rmsg->offset += p_rcb->p_rmsg->len;
      req_continue = true;
    } else if (p_rcb->p_rmsg == NULL) {
      /* Received a CONTINUE/END, but no corresponding START
                      (or previous fragmented response was dropped) */
      AVRC_TRACE_DEBUG(
          "Received a CONTINUE/END without no corresponding START \
                                (or previous fragmented response was dropped)");
      drop_code = 5;
      osi_free(p_pkt);
      *pp_pkt = NULL;
    } else {
      /* get size of buffer holding assembled message */
      /*
       * NOTE: The buffer is allocated above at the beginning of the
       * reassembly, and is always of size BT_DEFAULT_BUFFER_SIZE.
       */
      uint16_t buf_len = BT_DEFAULT_BUFFER_SIZE - sizeof(BT_HDR);
      /* adjust offset and len of fragment for header byte */
      p_pkt->offset += (AVRC_VENDOR_HDR_SIZE + AVRC_MIN_META_HDR_SIZE);
      p_pkt->len -= (AVRC_VENDOR_HDR_SIZE + AVRC_MIN_META_HDR_SIZE);
      /* verify length */
      if ((p_rcb->p_rmsg->offset + p_pkt->len) > buf_len) {
        AVRC_TRACE_WARNING(
            "Fragmented message too big! - report the partial message");
        p_pkt->len = buf_len - p_rcb->p_rmsg->offset;
        pkt_type = AVRC_PKT_END;
        buf_overflow = true;
      }

      /* copy contents of p_pkt to p_rx_msg */
      memcpy((uint8_t*)(p_rcb->p_rmsg + 1) + p_rcb->p_rmsg->offset,
             (uint8_t*)(p_pkt + 1) + p_pkt->offset, p_pkt->len);

      if (pkt_type == AVRC_PKT_END) {
        p_rcb->p_rmsg->offset = p_rcb->rasm_offset;
        p_rcb->p_rmsg->len += p_pkt->len;
        p_pkt_new = p_rcb->p_rmsg;
        p_rcb->rasm_offset = 0;
        p_rcb->p_rmsg = NULL;
        p_msg->p_vendor_data = (uint8_t*)(p_pkt_new + 1) + p_pkt_new->offset;
        p_msg->hdr.ctype = p_msg->p_vendor_data[0] & AVRC_CTYPE_MASK;
        /* 6 = ctype, subunit*, opcode & CO_ID */
        p_msg->p_vendor_data += AVRC_VENDOR_HDR_SIZE;
        p_msg->vendor_len = p_pkt_new->len - AVRC_VENDOR_HDR_SIZE;
        p_data = p_msg->p_vendor_data + 1; /* skip pdu */
        *p_data++ = AVRC_PKT_SINGLE;
        UINT16_TO_BE_STREAM(p_data,
                            (p_msg->vendor_len - AVRC_MIN_META_HDR_SIZE));
        AVRC_TRACE_DEBUG("end frag:%d, total len:%d, offset:%d", p_pkt->len,
                         p_pkt_new->len, p_pkt_new->offset);
      } else {
        p_rcb->p_rmsg->offset += p_pkt->len;
        p_rcb->p_rmsg->len += p_pkt->len;
        p_pkt_new = NULL;
        req_continue = true;
      }
      osi_free(p_pkt);
      *pp_pkt = p_pkt_new;
    }
  }

  if (cr == AVCT_CMD) {
    p_rsp = avrc_proc_vendor_command(handle, label, *pp_pkt, p_msg);
    if (p_rsp) {
      AVCT_MsgReq(handle, label, AVCT_RSP, p_rsp);
      osi_free_and_reset((void**)pp_pkt);
      drop_code = 3;
    } else if (p_msg->hdr.opcode == AVRC_OP_DROP) {
      drop_code = 1;
    } else if (p_msg->hdr.opcode == AVRC_OP_DROP_N_FREE)
      drop_code = 4;

  } else if (cr == AVCT_RSP) {
    if (req_continue) {
      avrc_cmd.pdu = AVRC_PDU_REQUEST_CONTINUATION_RSP;
      drop_code = 2;
    } else if (buf_overflow) {
      /* Incoming message too big to fit in BT_DEFAULT_BUFFER_SIZE. Send abort
       * to peer  */
      avrc_cmd.pdu = AVRC_PDU_ABORT_CONTINUATION_RSP;
      drop_code = 4;
    } else {
      return drop_code;
    }
    avrc_cmd.status = AVRC_STS_NO_ERROR;
    avrc_cmd.target_pdu = p_rcb->rasm_pdu;

    tAVRC_COMMAND avrc_command;
    avrc_command.continu = avrc_cmd;
    status = AVRC_BldCommand(&avrc_command, &p_cmd);
    if (status == AVRC_STS_NO_ERROR) {
      AVRC_MsgReq(handle, (uint8_t)(label), AVRC_CMD_CTRL, p_cmd);
    }
  }

  return drop_code;
}

/******************************************************************************
 *
 * Function         avrc_msg_cback
 *
 * Description      This is the callback function used by AVCTP to report
 *                  received AV control messages.
 *
 * Returns          Nothing.
 *
 *****************************************************************************/
static void avrc_msg_cback(uint8_t handle, uint8_t label, uint8_t cr,
                           BT_HDR* p_pkt) {
  uint8_t opcode;
  tAVRC_MSG msg;
  uint8_t* p_data;
  uint8_t* p_begin;
  bool drop = false;
  bool do_free = true;
  BT_HDR* p_rsp = NULL;
  uint8_t* p_rsp_data;
  int xx;
  bool reject = false;
  const char* p_drop_msg = "dropped";
  tAVRC_MSG_VENDOR* p_msg = &msg.vendor;

  if (cr == AVCT_CMD && (p_pkt->layer_specific & AVCT_DATA_CTRL &&
                         AVRC_PACKET_LEN < sizeof(p_pkt->len))) {
    /* Ignore the invalid AV/C command frame */
    p_drop_msg = "dropped - too long AV/C cmd frame size";
    osi_free(p_pkt);
    return;
  }

  if (cr == AVCT_REJ) {
    /* The peer thinks that this PID is no longer open - remove this handle */
    /*  */
    osi_free(p_pkt);
    AVCT_RemoveConn(handle);
    return;
  } else if (cr == AVCT_RSP) {
    /* Received response. Stop command timeout timer */
    AVRC_TRACE_DEBUG("AVRC: stopping timer (handle=0x%02x)", handle);
    alarm_cancel(avrc_cb.ccb_int[handle].tle);
  }

  p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
  memset(&msg, 0, sizeof(tAVRC_MSG));

  if (p_pkt->layer_specific == AVCT_DATA_BROWSE) {
    opcode = AVRC_OP_BROWSE;
    msg.browse.hdr.ctype = cr;
    msg.browse.p_browse_data = p_data;
    msg.browse.browse_len = p_pkt->len;
    msg.browse.p_browse_pkt = p_pkt;
  } else {
    if (p_pkt->len < AVRC_AVC_HDR_SIZE) {
      android_errorWriteLog(0x534e4554, "111803925");
      AVRC_TRACE_WARNING("%s: message length %d too short: must be at least %d",
                         __func__, p_pkt->len, AVRC_AVC_HDR_SIZE);
      osi_free(p_pkt);
      return;
    }
    msg.hdr.ctype = p_data[0] & AVRC_CTYPE_MASK;
    AVRC_TRACE_DEBUG("%s handle:%d, ctype:%d, offset:%d, len: %d", __func__,
                     handle, msg.hdr.ctype, p_pkt->offset, p_pkt->len);
    msg.hdr.subunit_type =
        (p_data[1] & AVRC_SUBTYPE_MASK) >> AVRC_SUBTYPE_SHIFT;
    msg.hdr.subunit_id = p_data[1] & AVRC_SUBID_MASK;
    opcode = p_data[2];
  }

  if (((avrc_cb.ccb[handle].control & AVRC_CT_TARGET) && (cr == AVCT_CMD)) ||
      ((avrc_cb.ccb[handle].control & AVRC_CT_CONTROL) && (cr == AVCT_RSP))) {
    switch (opcode) {
      case AVRC_OP_UNIT_INFO:
        if (cr == AVCT_CMD) {
          /* send the response to the peer */
          p_rsp = avrc_copy_packet(p_pkt, AVRC_OP_UNIT_INFO_RSP_LEN);
          p_rsp_data = avrc_get_data_ptr(p_rsp);
          *p_rsp_data = AVRC_RSP_IMPL_STBL;
          /* check & set the offset. set response code, set subunit_type &
             subunit_id,
             set AVRC_OP_UNIT_INFO */
          /* 3 bytes: ctype, subunit*, opcode */
          p_rsp_data += AVRC_AVC_HDR_SIZE;
          *p_rsp_data++ = 7;
          /* Panel subunit & id=0 */
          *p_rsp_data++ = (AVRC_SUB_PANEL << AVRC_SUBTYPE_SHIFT);
          AVRC_CO_ID_TO_BE_STREAM(p_rsp_data, avrc_cb.ccb[handle].company_id);
          p_rsp->len =
              (uint16_t)(p_rsp_data - (uint8_t*)(p_rsp + 1) - p_rsp->offset);
          cr = AVCT_RSP;
          p_drop_msg = "auto respond";
        } else {
          /* parse response */
          if (p_pkt->len < AVRC_OP_UNIT_INFO_RSP_LEN) {
            AVRC_TRACE_WARNING(
                "%s: message length %d too short: must be at least %d",
                __func__, p_pkt->len, AVRC_OP_UNIT_INFO_RSP_LEN);
            android_errorWriteLog(0x534e4554, "79883824");
            drop = true;
            p_drop_msg = "UNIT_INFO_RSP too short";
            break;
          }
          p_data += 4; /* 3 bytes: ctype, subunit*, opcode + octet 3 (is 7)*/
          msg.unit.unit_type =
              (*p_data & AVRC_SUBTYPE_MASK) >> AVRC_SUBTYPE_SHIFT;
          msg.unit.unit = *p_data & AVRC_SUBID_MASK;
          p_data++;
          AVRC_BE_STREAM_TO_CO_ID(msg.unit.company_id, p_data);
        }
        break;

      case AVRC_OP_SUB_INFO:
        if (cr == AVCT_CMD) {
          /* send the response to the peer */
          p_rsp = avrc_copy_packet(p_pkt, AVRC_OP_SUB_UNIT_INFO_RSP_LEN);
          p_rsp_data = avrc_get_data_ptr(p_rsp);
          *p_rsp_data = AVRC_RSP_IMPL_STBL;
          /* check & set the offset. set response code, set (subunit_type &
             subunit_id),
             set AVRC_OP_SUB_INFO, set (page & extention code) */
          p_rsp_data += 4;
          /* Panel subunit & id=0 */
          *p_rsp_data++ = (AVRC_SUB_PANEL << AVRC_SUBTYPE_SHIFT);
          memset(p_rsp_data, AVRC_CMD_OPRND_PAD, AVRC_SUBRSP_OPRND_BYTES);
          p_rsp_data += AVRC_SUBRSP_OPRND_BYTES;
          p_rsp->len =
              (uint16_t)(p_rsp_data - (uint8_t*)(p_rsp + 1) - p_rsp->offset);
          cr = AVCT_RSP;
          p_drop_msg = "auto responded";
        } else {
          /* parse response */
          if (p_pkt->len < AVRC_OP_SUB_UNIT_INFO_RSP_LEN) {
            AVRC_TRACE_WARNING(
                "%s: message length %d too short: must be at least %d",
                __func__, p_pkt->len, AVRC_OP_SUB_UNIT_INFO_RSP_LEN);
            android_errorWriteLog(0x534e4554, "79883824");
            drop = true;
            p_drop_msg = "SUB_UNIT_INFO_RSP too short";
            break;
          }
          p_data += AVRC_AVC_HDR_SIZE; /* 3 bytes: ctype, subunit*, opcode */
          msg.sub.page =
              (*p_data++ >> AVRC_SUB_PAGE_SHIFT) & AVRC_SUB_PAGE_MASK;
          xx = 0;
          while (*p_data != AVRC_CMD_OPRND_PAD && xx < AVRC_SUB_TYPE_LEN) {
            msg.sub.subunit_type[xx] = *p_data++ >> AVRC_SUBTYPE_SHIFT;
            if (msg.sub.subunit_type[xx] == AVRC_SUB_PANEL)
              msg.sub.panel = true;
            xx++;
          }
        }
        break;

      case AVRC_OP_VENDOR: {
        p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
        p_begin = p_data;
        if (p_pkt->len <
            AVRC_VENDOR_HDR_SIZE) /* 6 = ctype, subunit*, opcode & CO_ID */
        {
          if (cr == AVCT_CMD)
            reject = true;
          else
            drop = true;
          break;
        }
        p_data += AVRC_AVC_HDR_SIZE; /* skip the first 3 bytes: ctype, subunit*,
                                        opcode */
        AVRC_BE_STREAM_TO_CO_ID(p_msg->company_id, p_data);
        p_msg->p_vendor_data = p_data;
        p_msg->vendor_len = p_pkt->len - (p_data - p_begin);

        uint8_t drop_code = 0;
        if (p_msg->company_id == AVRC_CO_METADATA) {
          /* Validate length for metadata message */
          if (p_pkt->len < (AVRC_VENDOR_HDR_SIZE + AVRC_MIN_META_HDR_SIZE)) {
            if (cr == AVCT_CMD)
              reject = true;
            else
              drop = true;
            break;
          }

          /* Check+handle fragmented messages */
          drop_code = avrc_proc_far_msg(handle, label, cr, &p_pkt, p_msg);
          if (drop_code > 0) drop = true;
        }
        if (drop_code > 0) {
          if (drop_code != 4) do_free = false;
          switch (drop_code) {
            case 1:
              p_drop_msg = "sent_frag";
              break;
            case 2:
              p_drop_msg = "req_cont";
              break;
            case 3:
              p_drop_msg = "sent_frag3";
              break;
            case 4:
              p_drop_msg = "sent_frag_free";
              break;
            default:
              p_drop_msg = "sent_fragd";
          }
        }
        /* If vendor response received, and did not ask for continuation */
        /* then check queue for addition commands to send */
        if ((cr == AVCT_RSP) && (drop_code != 2)) {
          avrc_send_next_vendor_cmd(handle);
        }
      } break;

      case AVRC_OP_PASS_THRU:
        if (p_pkt->len < 5) /* 3 bytes: ctype, subunit*, opcode & op_id & len */
        {
          if (cr == AVCT_CMD)
            reject = true;
          else
            drop = true;
          break;
        }
        p_data += AVRC_AVC_HDR_SIZE; /* skip the first 3 bytes: ctype, subunit*,
                                        opcode */
        msg.pass.op_id = (AVRC_PASS_OP_ID_MASK & *p_data);
        if (AVRC_PASS_STATE_MASK & *p_data)
          msg.pass.state = true;
        else
          msg.pass.state = false;
        p_data++;
        msg.pass.pass_len = *p_data++;
        if (msg.pass.pass_len != p_pkt->len - 5)
          msg.pass.pass_len = p_pkt->len - 5;
        if (msg.pass.pass_len)
          msg.pass.p_pass_data = p_data;
        else
          msg.pass.p_pass_data = NULL;
        break;

      case AVRC_OP_BROWSE:
        /* If browse response received, then check queue for addition commands
         * to send */
        if (cr == AVCT_RSP) {
          avrc_send_next_vendor_cmd(handle);
        }
        break;

      default:
        if ((avrc_cb.ccb[handle].control & AVRC_CT_TARGET) &&
            (cr == AVCT_CMD)) {
          /* reject unsupported opcode */
          reject = true;
        }
        drop = true;
        break;
    }
  } else /* drop the event */
  {
    if (opcode != AVRC_OP_BROWSE) drop = true;
  }

  if (reject) {
    /* reject unsupported opcode */
    p_rsp = avrc_copy_packet(p_pkt, AVRC_OP_REJ_MSG_LEN);
    p_rsp_data = avrc_get_data_ptr(p_rsp);
    *p_rsp_data = AVRC_RSP_REJ;
    p_drop_msg = "rejected";
    cr = AVCT_RSP;
    drop = true;
  }

  if (p_rsp) {
    /* set to send response right away */
    AVCT_MsgReq(handle, label, cr, p_rsp);
    drop = true;
  }

  if (!drop) {
    msg.hdr.opcode = opcode;
    avrc_cb.ccb[handle].msg_cback.Run(handle, label, opcode, &msg);
  } else {
    AVRC_TRACE_WARNING("%s %s msg handle:%d, control:%d, cr:%d, opcode:x%x",
                       __func__, p_drop_msg, handle,
                       avrc_cb.ccb[handle].control, cr, opcode);
  }

  if (opcode == AVRC_OP_BROWSE && msg.browse.p_browse_pkt == NULL) {
    do_free = false;
  }

  if (do_free) osi_free(p_pkt);
}

/******************************************************************************
 *
 * Function         avrc_pass_msg
 *
 * Description      Compose a PASS THROUGH command according to p_msg
 *
 *                  Input Parameters:
 *                      p_msg: Pointer to PASS THROUGH message structure.
 *
 *                  Output Parameters:
 *                      None.
 *
 * Returns          pointer to a valid GKI buffer if successful.
 *                  NULL if p_msg is NULL.
 *
 *****************************************************************************/
static BT_HDR* avrc_pass_msg(tAVRC_MSG_PASS* p_msg) {
  CHECK(p_msg != NULL);
  CHECK(AVRC_CMD_BUF_SIZE > (AVRC_MIN_CMD_LEN + p_msg->pass_len));

  BT_HDR* p_cmd = (BT_HDR*)osi_malloc(AVRC_CMD_BUF_SIZE);
  p_cmd->offset = AVCT_MSG_OFFSET;
  p_cmd->layer_specific = AVCT_DATA_CTRL;

  uint8_t* p_data = (uint8_t*)(p_cmd + 1) + p_cmd->offset;
  *p_data++ = (p_msg->hdr.ctype & AVRC_CTYPE_MASK);
  *p_data++ = (AVRC_SUB_PANEL << AVRC_SUBTYPE_SHIFT); /* Panel subunit & id=0 */
  *p_data++ = AVRC_OP_PASS_THRU;
  *p_data = (AVRC_PASS_OP_ID_MASK & p_msg->op_id);
  if (p_msg->state) *p_data |= AVRC_PASS_STATE_MASK;
  p_data++;

  if (p_msg->op_id == AVRC_ID_VENDOR) {
    *p_data++ = p_msg->pass_len;
    if (p_msg->pass_len && p_msg->p_pass_data) {
      memcpy(p_data, p_msg->p_pass_data, p_msg->pass_len);
      p_data += p_msg->pass_len;
    }
  } else {
    /* set msg len to 0 for other op_id */
    *p_data++ = 0;
  }
  p_cmd->len = (uint16_t)(p_data - (uint8_t*)(p_cmd + 1) - p_cmd->offset);

  return p_cmd;
}

/******************************************************************************
 *
 * Function         AVRC_Open
 *
 * Description      This function is called to open a connection to AVCTP.
 *                  The connection can be either an initiator or acceptor, as
 *                  determined by the p_ccb->stream parameter.
 *                  The connection can be a target, a controller or for both
 *                  role, as determined by the p_ccb->control parameter.
 *                  By definition, a target connection is an acceptor connection
 *                  that waits for an incoming AVCTP connection from the peer.
 *                  The connection remains available to the application until
 *                  the application closes it by calling AVRC_Close().  The
 *                  application does not need to reopen the connection after an
 *                  AVRC_CLOSE_IND_EVT is received.
 *
 *                  Input Parameters:
 *                      p_ccb->company_id: Company Identifier.
 *
 *                      p_ccb->p_ctrl_cback:  Pointer to control callback
 *                                            function.
 *
 *                      p_ccb->p_msg_cback:  Pointer to message callback
 *                                            function.
 *
 *                      p_ccb->conn: AVCTP connection role.  This is set to
 *                      AVCTP_INT for initiator connections and AVCTP_ACP
 *                      for acceptor connections.
 *
 *                      p_ccb->control: Control role.  This is set to
 *                      AVRC_CT_TARGET for target connections, AVRC_CT_CONTROL
 *                      for control connections or
 *                      (AVRC_CT_TARGET|AVRC_CT_CONTROL)
 *                      for connections that support both roles.
 *
 *                      peer_addr: BD address of peer device.  This value is
 *                      only used for initiator connections; for acceptor
 *                      connections it can be set to NULL.
 *
 *                  Output Parameters:
 *                      p_handle: Pointer to handle.  This parameter is only
 *                                valid if AVRC_SUCCESS is returned.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_NO_RESOURCES if there are not enough resources to open
 *                  the connection.
 *
 *****************************************************************************/
uint16_t AVRC_Open(uint8_t* p_handle, tAVRC_CONN_CB* p_ccb,
                   const RawAddress& peer_addr) {
  uint16_t status;
  tAVCT_CC cc;

  cc.p_ctrl_cback = avrc_ctrl_cback;         /* Control callback */
  cc.p_msg_cback = avrc_msg_cback;           /* Message callback */
  cc.pid = UUID_SERVCLASS_AV_REMOTE_CONTROL; /* Profile ID */
  cc.role = p_ccb->conn;                     /* Initiator/acceptor role */
  cc.control = p_ccb->control;               /* Control role (Control/Target) */

  status = AVCT_CreateConn(p_handle, &cc, peer_addr);
  if (status == AVCT_SUCCESS) {
    avrc_cb.ccb[*p_handle] = *p_ccb;
    memset(&avrc_cb.ccb_int[*p_handle], 0, sizeof(tAVRC_CONN_INT_CB));
    memset(&avrc_cb.fcb[*p_handle], 0, sizeof(tAVRC_FRAG_CB));
    memset(&avrc_cb.rcb[*p_handle], 0, sizeof(tAVRC_RASM_CB));
    avrc_cb.ccb_int[*p_handle].tle = alarm_new("avrcp.commandTimer");
    avrc_cb.ccb_int[*p_handle].cmd_q = fixed_queue_new(SIZE_MAX);
  }
  AVRC_TRACE_DEBUG("%s role: %d, control:%d status:%d, handle:%d", __func__,
                   cc.role, cc.control, status, *p_handle);

  return status;
}

/******************************************************************************
 *
 * Function         AVRC_Close
 *
 * Description      Close a connection opened with AVRC_Open().
 *                  This function is called when the
 *                  application is no longer using a connection.
 *
 *                  Input Parameters:
 *                      handle: Handle of this connection.
 *
 *                  Output Parameters:
 *                      None.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_BAD_HANDLE if handle is invalid.
 *
 *****************************************************************************/
uint16_t AVRC_Close(uint8_t handle) {
  AVRC_TRACE_DEBUG("%s handle:%d", __func__, handle);
  avrc_flush_cmd_q(handle);
  return AVCT_RemoveConn(handle);
}

/******************************************************************************
 *
 * Function         AVRC_OpenBrowse
 *
 * Description      This function is called to open a browsing connection to
 *                  AVCTP. The connection can be either an initiator or
 *                  acceptor, as determined by the p_conn_role.
 *                  The handle is returned by a previous call to AVRC_Open.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_NO_RESOURCES if there are not enough resources to open
 *                  the connection.
 *
 *****************************************************************************/
uint16_t AVRC_OpenBrowse(uint8_t handle, uint8_t conn_role) {
  return AVCT_CreateBrowse(handle, conn_role);
}

/******************************************************************************
 *
 * Function         AVRC_CloseBrowse
 *
 * Description      Close a connection opened with AVRC_OpenBrowse().
 *                  This function is called when the
 *                  application is no longer using a connection.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_BAD_HANDLE if handle is invalid.
 *
 *****************************************************************************/
uint16_t AVRC_CloseBrowse(uint8_t handle) { return AVCT_RemoveBrowse(handle); }

/******************************************************************************
 *
 * Function         AVRC_MsgReq
 *
 * Description      This function is used to send the AVRCP byte stream in p_pkt
 *                  down to AVCTP.
 *
 *                  It is expected that p_pkt->offset is at least
 *                  AVCT_MSG_OFFSET
 *                  p_pkt->layer_specific is AVCT_DATA_CTRL or AVCT_DATA_BROWSE
 *                  p_pkt->event is AVRC_OP_VENDOR, AVRC_OP_PASS_THRU or
 *                  AVRC_OP_BROWSE
 *                  The above BT_HDR settings are set by the AVRC_Bld*
 *                  functions.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_BAD_HANDLE if handle is invalid.
 *
 *****************************************************************************/
uint16_t AVRC_MsgReq(uint8_t handle, uint8_t label, uint8_t ctype,
                     BT_HDR* p_pkt) {
  uint8_t* p_data;
  uint8_t cr = AVCT_CMD;
  bool chk_frag = true;
  uint8_t* p_start = NULL;
  tAVRC_FRAG_CB* p_fcb;
  uint16_t len;
  uint16_t status;
  uint8_t msg_mask = 0;
  uint16_t peer_mtu;

  if (!p_pkt) return AVRC_BAD_PARAM;

  AVRC_TRACE_DEBUG("%s handle = %u label = %u ctype = %u len = %d", __func__,
                   handle, label, ctype, p_pkt->len);

  if (ctype >= AVRC_RSP_NOT_IMPL) cr = AVCT_RSP;

  if (p_pkt->event == AVRC_OP_VENDOR) {
    /* add AVRCP Vendor Dependent headers */
    p_start = ((uint8_t*)(p_pkt + 1) + p_pkt->offset);
    p_pkt->offset -= AVRC_VENDOR_HDR_SIZE;
    p_pkt->len += AVRC_VENDOR_HDR_SIZE;
    p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
    *p_data++ = (ctype & AVRC_CTYPE_MASK);
    *p_data++ = (AVRC_SUB_PANEL << AVRC_SUBTYPE_SHIFT);
    *p_data++ = AVRC_OP_VENDOR;
    AVRC_CO_ID_TO_BE_STREAM(p_data, AVRC_CO_METADATA);

    /* Check if this is a AVRC_PDU_REQUEST_CONTINUATION_RSP */
    if (cr == AVCT_CMD) {
      msg_mask |= AVRC_MSG_MASK_IS_VENDOR_CMD;

      if ((*p_start == AVRC_PDU_REQUEST_CONTINUATION_RSP) ||
          (*p_start == AVRC_PDU_ABORT_CONTINUATION_RSP)) {
        msg_mask |= AVRC_MSG_MASK_IS_CONTINUATION_RSP;
      }
    }
  } else if (p_pkt->event == AVRC_OP_PASS_THRU) {
    /* add AVRCP Pass Through headers */
    p_start = ((uint8_t*)(p_pkt + 1) + p_pkt->offset);
    p_pkt->offset -= AVRC_PASS_THRU_SIZE;
    p_pkt->len += AVRC_PASS_THRU_SIZE;
    p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
    *p_data++ = (ctype & AVRC_CTYPE_MASK);
    *p_data++ = (AVRC_SUB_PANEL << AVRC_SUBTYPE_SHIFT);
    *p_data++ = AVRC_OP_PASS_THRU; /* opcode */
    *p_data++ = AVRC_ID_VENDOR;    /* operation id */
    *p_data++ = 5;                 /* operation data len */
    AVRC_CO_ID_TO_BE_STREAM(p_data, AVRC_CO_METADATA);
  } else {
    chk_frag = false;
    if (p_pkt->layer_specific == AVCT_DATA_BROWSE) {
      peer_mtu = AVCT_GetBrowseMtu(handle);
    } else {
      peer_mtu = AVCT_GetPeerMtu(handle);
    }
    if (p_pkt->len > (peer_mtu - AVCT_HDR_LEN_SINGLE)) {
      AVRC_TRACE_ERROR(
          "%s bigger than peer mtu (p_pkt->len(%d) > peer_mtu(%d-%d))",
          __func__, p_pkt->len, peer_mtu, AVCT_HDR_LEN_SINGLE);
      osi_free(p_pkt);
      return AVRC_MSG_TOO_BIG;
    }
  }

  /* abandon previous fragments */
  p_fcb = &avrc_cb.fcb[handle];

  if (p_fcb == NULL) {
    AVRC_TRACE_ERROR("%s p_fcb is NULL", __func__);
    osi_free(p_pkt);
    return AVRC_NOT_OPEN;
  }

  if (p_fcb->frag_enabled) p_fcb->frag_enabled = false;

  osi_free_and_reset((void**)&p_fcb->p_fmsg);

  /* AVRCP spec has not defined any control channel commands that needs
   * fragmentation at this level
   * check for fragmentation only on the response */
  if ((cr == AVCT_RSP) && (chk_frag)) {
    if (p_pkt->len > AVRC_MAX_CTRL_DATA_LEN) {
      int offset_len = MAX(AVCT_MSG_OFFSET, p_pkt->offset);
      BT_HDR* p_pkt_new =
          (BT_HDR*)osi_malloc(AVRC_PACKET_LEN + offset_len + BT_HDR_SIZE);
      if (p_start != NULL) {
        p_fcb->frag_enabled = true;
        p_fcb->p_fmsg = p_pkt;
        p_fcb->frag_pdu = *p_start;
        p_pkt = p_pkt_new;
        p_pkt_new = p_fcb->p_fmsg;
        p_pkt->len = AVRC_MAX_CTRL_DATA_LEN;
        p_pkt->offset = p_pkt_new->offset;
        p_pkt->layer_specific = p_pkt_new->layer_specific;
        p_pkt->event = p_pkt_new->event;
        p_data = (uint8_t*)(p_pkt + 1) + p_pkt->offset;
        p_start -= AVRC_VENDOR_HDR_SIZE;
        memcpy(p_data, p_start, AVRC_MAX_CTRL_DATA_LEN);
        /* use AVRC start packet type */
        p_data += AVRC_VENDOR_HDR_SIZE;
        p_data++; /* pdu */
        *p_data++ = AVRC_PKT_START;

        /* 4 pdu, pkt_type & len */
        len = (AVRC_MAX_CTRL_DATA_LEN - AVRC_VENDOR_HDR_SIZE -
               AVRC_MIN_META_HDR_SIZE);
        UINT16_TO_BE_STREAM(p_data, len);

        /* prepare the left over for as an end fragment */
        avrc_prep_end_frag(handle);
        AVRC_TRACE_DEBUG("%s p_pkt len:%d/%d, next len:%d", __func__,
                         p_pkt->len, len, p_fcb->p_fmsg->len);
      } else {
        /* TODO: Is this "else" block valid? Remove it? */
        AVRC_TRACE_ERROR("%s no buffers for fragmentation", __func__);
        osi_free(p_pkt);
        return AVRC_NO_RESOURCES;
      }
    }
  } else if ((p_pkt->event == AVRC_OP_VENDOR) && (cr == AVCT_CMD) &&
             (avrc_cb.ccb_int[handle].flags & AVRC_CB_FLAGS_RSP_PENDING) &&
             !(msg_mask & AVRC_MSG_MASK_IS_CONTINUATION_RSP)) {
    /* If we are sending a vendor specific command, and a response is pending,
     * then enqueue the command until the response has been received.
     * This is to interop with TGs that abort sending responses whenever a new
     * command
     * is received (exception is continuation request command
     * must sent that to get additional response frags) */
    AVRC_TRACE_DEBUG(
        "AVRC: Enqueuing command 0x%08x (handle=0x%02x, label=0x%02x)", p_pkt,
        handle, label);

    /* label in BT_HDR (will need this later when the command is dequeued) */
    p_pkt->layer_specific = (label << 8) | (p_pkt->layer_specific & 0xFF);

    /* Enqueue the command */
    fixed_queue_enqueue(avrc_cb.ccb_int[handle].cmd_q, p_pkt);
    return AVRC_SUCCESS;
  }

  /* Send the message */
  status = AVCT_MsgReq(handle, label, cr, p_pkt);
  if ((status == AVCT_SUCCESS) && (cr == AVCT_CMD)) {
    /* If a command was successfully sent, indicate that a response is pending
     */
    avrc_cb.ccb_int[handle].flags |= AVRC_CB_FLAGS_RSP_PENDING;

    /* Start command timer to wait for response */
    avrc_start_cmd_timer(handle, label, msg_mask);
  }

  return status;
}

/******************************************************************************
 *
 * Function         AVRC_PassCmd
 *
 * Description      Send a PASS THROUGH command to the peer device.  This
 *                  function can only be called for controller role connections.
 *                  Any response message from the peer is passed back through
 *                  the tAVRC_MSG_CBACK callback function.
 *
 *                  Input Parameters:
 *                      handle: Handle of this connection.
 *
 *                      label: Transaction label.
 *
 *                      p_msg: Pointer to PASS THROUGH message structure.
 *
 *                  Output Parameters:
 *                      None.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_BAD_HANDLE if handle is invalid.
 *
 *****************************************************************************/
uint16_t AVRC_PassCmd(uint8_t handle, uint8_t label, tAVRC_MSG_PASS* p_msg) {
  BT_HDR* p_buf;
  uint16_t status = AVRC_NO_RESOURCES;
  if (!p_msg) return AVRC_BAD_PARAM;

  p_msg->hdr.ctype = AVRC_CMD_CTRL;
  p_buf = avrc_pass_msg(p_msg);
  if (p_buf) {
    status = AVCT_MsgReq(handle, label, AVCT_CMD, p_buf);
    if (status == AVCT_SUCCESS) {
      /* Start command timer to wait for response */
      avrc_start_cmd_timer(handle, label, 0);
    }
  }
  return (status);
}

/******************************************************************************
 *
 * Function         AVRC_PassRsp
 *
 * Description      Send a PASS THROUGH response to the peer device.  This
 *                  function can only be called for target role connections.
 *                  This function must be called when a PASS THROUGH command
 *                  message is received from the peer through the
 *                  tAVRC_MSG_CBACK callback function.
 *
 *                  Input Parameters:
 *                      handle: Handle of this connection.
 *
 *                      label: Transaction label.  Must be the same value as
 *                      passed with the command message in the callback
 *                      function.
 *
 *                      p_msg: Pointer to PASS THROUGH message structure.
 *
 *                  Output Parameters:
 *                      None.
 *
 * Returns          AVRC_SUCCESS if successful.
 *                  AVRC_BAD_HANDLE if handle is invalid.
 *
 *****************************************************************************/
uint16_t AVRC_PassRsp(uint8_t handle, uint8_t label, tAVRC_MSG_PASS* p_msg) {
  BT_HDR* p_buf;
  if (!p_msg) return AVRC_BAD_PARAM;

  p_buf = avrc_pass_msg(p_msg);
  if (p_buf) return AVCT_MsgReq(handle, label, AVCT_RSP, p_buf);
  return AVRC_NO_RESOURCES;
}
