/*
 * Copyright (c) 2007, Vanderbilt University
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
 *
 * Author: Miklos Maroti
 */

#include <Tasklet.h>
#include <RadioAssert.h>

generic module CsmaLayerP()
{
	provides
	{
		interface RadioSend;
	}

	uses
	{
		interface CsmaConfig as Config;

		interface RadioSend as SubSend;
		interface RadioCCA as SubCCA;
	}
}

implementation
{
	tasklet_norace message_t *txMsg;

	tasklet_norace uint8_t state;
	enum
	{
		STATE_READY = 0,
		STATE_CCA_WAIT = 1,
		STATE_SEND = 2,
	};

	tasklet_async event void SubSend.ready()
	{
		if( state == STATE_READY )
			signal RadioSend.ready();
	}

	tasklet_async command error_t RadioSend.send(message_t* msg)
	{
		error_t error;

		if( state == STATE_READY )
		{
			if( call Config.requiresSoftwareCCA(msg) )
			{
				txMsg = msg;

				if( (error = call SubCCA.request()) == SUCCESS )
					state = STATE_CCA_WAIT;
			}
			else if( (error = call SubSend.send(msg)) == SUCCESS )
				state = STATE_SEND;
		}
		else
			error = EBUSY;

		return error;
	}

	tasklet_async event void SubCCA.done(error_t error)
	{
		RADIO_ASSERT( state == STATE_CCA_WAIT );

		if( error == SUCCESS && (error = call SubSend.send(txMsg)) == SUCCESS )
			state = STATE_SEND;
		else
		{
			state = STATE_READY;
			signal RadioSend.sendDone(EBUSY);
		}
	}

	tasklet_async event void SubSend.sendDone(error_t error)
	{
		RADIO_ASSERT( state == STATE_SEND );

		state = STATE_READY;
		signal RadioSend.sendDone(error);
	}
}
