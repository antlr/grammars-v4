/*
 * Copyright (c) 2006, Technische Universitaet Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * - Neither the name of the Technische Universitaet Berlin nor the names
 *   of its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * - Revision -------------------------------------------------------------
 * $Revision: 1.4 $
 * $Date: 2006/12/12 18:22:49 $
 * @author: Jan Hauer
 * ========================================================================
 */

/**
 * 
 * Sensing demo application. See README.txt file in this directory for usage
 * instructions and have a look at tinyos-2.x/doc/html/tutorial/lesson5.html
 * for a general tutorial on sensing in TinyOS.
 *
 * @author Jan Hauer
 */

#include "Timer.h"

module SenseC
{
  uses {
    interface Boot;
    interface Leds;
    interface Timer<TMilli>;
    interface Read<uint16_t>;
  }
}
implementation
{
  // sampling frequency in binary milliseconds
  #define SAMPLING_FREQUENCY 100
  
  event void Boot.booted() {
    call Timer.startPeriodic(SAMPLING_FREQUENCY);
  }

  event void Timer.fired() 
  {
    call Read.read();
  }

  event void Read.readDone(error_t result, uint16_t data) 
  {
    if (result == SUCCESS){
      if (data & 0x0004)
        call Leds.led2On();
      else
        call Leds.led2Off();
      if (data & 0x0002)
        call Leds.led1On();
      else
        call Leds.led1Off();
      if (data & 0x0001)
        call Leds.led0On();
      else
        call Leds.led0Off();
    }
  }
}
