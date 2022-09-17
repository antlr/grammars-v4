/*
 * Copyright (c) 2010 Johns Hopkins University. All rights reserved.
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
 * - Neither the name of the copyright holder nor the names of
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
 */

/**
 * RPLDAORoutingEngineC.nc
 * @author JeongGil Ko (John) <jgko@cs.jhu.edu>
 */

#include <RPL.h>
#include <lib6lowpan/ip_malloc.h>
#include <lib6lowpan/in_cksum.h>
#include <lib6lowpan/ip.h>

configuration RPLDAORoutingEngineC{
  provides {
    interface StdControl;
    interface RPLDAORoutingEngine;
  }
  uses {
    interface IP as ICMP_RA[uint8_t code];
  }
} implementation{
  components new RPLDAORoutingEngineP() as DAORouting;
  components MainC, RandomC;
  components new TimerMilliC() as DelayDAOTimer;
  components new TimerMilliC() as GenerateDAOTimer;
  components new TimerMilliC() as RemoveTimer;
  components IPAddressC;
  components RPLRankC;
  components RPLRoutingEngineC;
  components IPStackC;

  StdControl = DAORouting;
  RPLDAORoutingEngine = DAORouting;
  DAORouting.IP_DAO = ICMP_RA[ICMPV6_CODE_DAO];

  DAORouting.DelayDAOTimer -> DelayDAOTimer;
  DAORouting.GenerateDAOTimer -> GenerateDAOTimer;
  DAORouting.RemoveTimer -> RemoveTimer;
  DAORouting.Random -> RandomC;
  DAORouting.IPAddress -> IPAddressC;
  DAORouting.RPLRouteInfo -> RPLRoutingEngineC;
  DAORouting.RootControl -> RPLRoutingEngineC;
  DAORouting.ForwardingTable -> IPStackC;

  components new QueueC(dao_entry_t*, RPL_QUEUE_SIZE) as SendQueueP;
  DAORouting.SendQueue -> SendQueueP;

  components new PoolC(dao_entry_t, RPL_QUEUE_SIZE) as SendPoolP;
  DAORouting.SendPool -> SendPoolP;

  components IPPacketC;
  DAORouting.IPPacket -> IPPacketC;
}
