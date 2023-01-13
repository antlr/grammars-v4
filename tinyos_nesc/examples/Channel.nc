/*
 * Copyright (c) 2008 Rincon Research Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the Rincon Research Corporation nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * RINCON RESEARCH OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE
 */
 
/**
 * Open or close a channel and retrieve the open status of the channel.
 * 
 * @author Danny Park
 */

interface Channel {
  /**
   * Open the channel.
   * 
   * @return SUCCESS if the openDone event will fire in the future.
   */
  command error_t open();
  
  /**
   * Close the channel.
   * 
   * @return SUCCESS if the closeDone event will fire in the future.
   */
  command error_t close();
  
  /**
   * Determine if the channel is open.
   * 
   * @return TRUE if the channel is open, FALSE otherwise.
   */
  command bool isOpen();
  
  /**
   * Notify the caller that the channel has been opened.
   * 
   * @param error SUCCESS if the channel was opened without any problems.
   */
  event void openDone(error_t error);
  
  /**
   * Notify the caller that the channel has been closed.
   * 
   * @param error SUCCESS if the channel was closed without any problems.
   */
  event void closeDone(error_t error);
}
