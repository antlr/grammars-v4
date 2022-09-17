/* $Id: LinkEstimator.nc,v 1.6 2010-06-29 22:07:49 scipio Exp $ */
/*
 * Copyright (c) 2005 The Regents of the University  of California.  
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the University of California nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/** Provides an additive quality measure for a neighbor. The
 * provided quality increases when the true link quality increases.
 *  @author Rodrigo Fonseca
 *  @author Omprakash Gnawali
 *  @date   $Date: 2010-06-29 22:07:49 $
 */

/* Quality of a link is defined by the implementor of this interface.
 * It could be ETX, PRR, etc.
 */

interface LinkEstimator {
  
  /* get bi-directional link quality for link to the neighbor */
  command uint16_t getLinkQuality(uint16_t neighbor);

  /* get quality of the link from neighbor to this node */
  command uint16_t getReverseQuality(uint16_t neighbor);

  /* get quality of the link from this node to the neighbor */
  command uint16_t getForwardQuality(uint16_t neighbor);

  /* insert this neighbor into the neighbor table */
  command error_t insertNeighbor(am_addr_t neighbor);

  /* pin a neighbor so that it does not get evicted */
  command error_t pinNeighbor(am_addr_t neighbor);

  /* pin a neighbor so that it does not get evicted */
  command error_t unpinNeighbor(am_addr_t neighbor);

  /* called when an acknowledgement is received; sign of a successful
     data transmission; to update forward link quality */
  command error_t txAck(am_addr_t neighbor);

  /* called when an acknowledgement is not received; could be due to
     data pkt or acknowledgement loss; to update forward link quality */
  command error_t txNoAck(am_addr_t neighbor);

  /* called when the parent changes; clear state about data-driven link quality  */
  command error_t clearDLQ(am_addr_t neighbor);

  /* signal when this neighbor is evicted from the neighbor table */
  event void evicted(am_addr_t neighbor);
}


