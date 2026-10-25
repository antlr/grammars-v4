/*
 * Copyright (c) 2012 Sestosenso
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
 * - Neither the name of the Sestosenso nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * SESTOSENSO OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/**
* Core component that allows the use of the digital outputs
* on the MDA 300 and 320 sensorboards.
* 
* This component does the following steps to set the digital
* output pins:
*   1. Request resource
*   2. Resource granted -> Write data through I2C bus
*   3. Write done -> Release resource and Signal done
* 
* @author Christopher Leung
* author Charles Elliott
* @modified June 5, 2008
* 
*	@modified September 2012 by Franco Di Persio, Sestosenso
*/

module MDA3XXADCP {
	provides {
		interface MDA300ADC;
	}
	uses {
		interface Leds; // as DebugLeds;
		interface I2CPacket<TI2CBasicAddr>;
		interface Resource;
	}
}
implementation {
	uint8_t I2C_data = 0xFF;	// Pin values
	uint8_t I2C_send;			// Buffer
	uint8_t I2C_read[2];	
	bool idle = TRUE;
	bool read = FALSE;
	
	uint8_t state_led;		//add in order to check value by leds - April 30, 2012
	
	task void writeToI2C() { 
		I2C_send = I2C_data;
		if ((call I2CPacket.write(I2C_START|I2C_STOP, 74, 1, (uint8_t*) &I2C_send)) != SUCCESS){		//0x94
			post writeToI2C();
		}
	}
	
	task void readToI2C() {		
		if ((call I2CPacket.read(I2C_START|I2C_STOP, 74, 2, (uint8_t*) &I2C_read)) != SUCCESS){		
			post readToI2C();
		}
	}
	
	task void signalReadyToSet() {
		signal MDA300ADC.readyToSet();
		idle = TRUE;
	}
	
	task void signalReadyToRead() {
		signal MDA300ADC.readyToRead();
		idle = TRUE;
	}
	
	task void getResource (){
		if (call Resource.request() != SUCCESS)
			post getResource();
	}
	
	/**
	 * Selects the ADC to read.
	 *  
	 *   CHANNEL SELECTION CONTROL
	 *	SD C2 C1 C0 CH0 CH1 CH2 CH3 CH4 CH5 CH6 CH7 COM
	 *	0  0  0  0  +IN –IN  —  —   —   —   —   —   —
	 *	0  0  0  1  —   —   +IN –IN —   —   —   —   —
	 *	0  0  1  0  —   —   —   —   +IN –IN —   —   —
	 *	0  0  1  1  —   —   —   —   —   —   +IN –IN —
	 *	0  1  0  0  –IN +IN —   —   —   —   —   —   —
	 *	0  1  0  1  —   —   –IN +IN —   —   —   —   —
	 *	0  1  1  0  —   —   —   —   –IN +IN —   —   —
	 *	0  1  1  1  —   —   —   —   —   —   –IN +IN —
	 *	1  0  0  0  +IN —   —   —   —   —   —   —   –IN
	 *	1  0  0  1  —   —   +IN —   —   —   —   —   –IN
	 *	1  0  1  0  —   —   —   —   +IN —   —   —   –IN
	 *	1  0  1  1  —   —   —   —   —   —   +IN —   –IN
	 *	1  1  0  0  —   +IN —   —   —   —   —   —   –IN
	 *	1  1  0  1  —   —   —   +IN —   —   —   —   –IN
	 *	1  1  1  0  —   —   —   —   —   +IN —   —   –IN
	 *	1  1  1  1  —   —   —   —   —   —   —   +IN –IN
	 *
	 * @param value channel selected.
	 * 
	 * @return SUCCESS if the component is free.
	 */
	command error_t MDA300ADC.selectPin (uint8_t pin){
		if (idle){
			idle = FALSE;
			read = FALSE;
			//pin = pin & 0xF0;
			I2C_data = pin;
// 			call Leds.led2Toggle();
			call Resource.request();//post getResource(); //call Resource.request();
			return SUCCESS;
		}
		return FAIL;
	}
	
	/**
	 * Reads the previously selected channel
	 *
	 * @param none
	 * 
	 * @return SUCCESS if the component is free.
	 */
	command error_t MDA300ADC.requestRead() {
		if (idle) {
			idle = FALSE;
			read = TRUE;
			call Resource.request();//post getResource(); //call Resource.request();
			return SUCCESS;
		}
		return FAIL;
	}
	
	/**
	 * Gets the selected channel.
	 *
	 * @note If get() is called during a write operation,
	 * the value that is being written will be returned.
	 *
	 * @return Pin values
	 */
	command uint8_t MDA300ADC.get() {
		uint8_t temp_I2C_data;
		temp_I2C_data = I2C_data;
		return temp_I2C_data;
	}
	
	/**
	 * Gets the pin values.
	 *
	 * @return Pin data value
	 */
	command uint16_t MDA300ADC.read() {	
		uint16_t temp_I2C_read = ((I2C_read[0] & 0xff) << 8) | (I2C_read[1] & 0xff);		//This is for the lenght = 2
		
		return temp_I2C_read;
	}
	
	event void Resource.granted() {		
		if (read){
			post readToI2C();
		}else{
			post writeToI2C();
		}
	}
	
	async event void I2CPacket.readDone(error_t error, uint16_t addr, uint8_t length, uint8_t* data) {
		call Resource.release();
// 		call Leds.led2Toggle();
		post signalReadyToRead();
	}
	
	async event void I2CPacket.writeDone(error_t error, uint16_t addr, uint8_t length, uint8_t* data) {
		call Resource.release();
// 		call Leds.led0Toggle();
		post signalReadyToSet();
	}
	
}
