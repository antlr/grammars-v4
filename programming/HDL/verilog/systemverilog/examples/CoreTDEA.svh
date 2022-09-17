/*
MIT License

Copyright (c) 2021 layup

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
class CoreTDEA extends DESTypes;
  protected _tL64 key1, key2, key3;
  protected bit key_dispatched;
  protected CoreDES des1, des2, des3;
  local static const eDESType this_type = TDEA;
  function new();
    key_dispatched = 0;
  endfunction
  function void setKey(_tL64 key1, key2, key3);
    this.key1 = key1;
    this.key2 = key2;
    this.key3 = key3;
    key_dispatched = 0;
  endfunction
  protected function void keyDispatch();
    if (!key_dispatched) begin
      if (des1 == null) des1 = new();
      if (des2 == null) des2 = new();
      des1.setKey(this.key1);
      des2.setKey(this.key2);
      if (this.key1 == this.key3) begin
        des3 = des1;
        `_LOG("!!Use 2-Key mode\n")
      end else begin
        if (des3 == null) des3 = new();
        des3.setKey(this.key3);
      end
      key_dispatched = 1;
    end
  endfunction : keyDispatch
  function uBlockT encrypt(uBlockT bdata);
    keyDispatch();
    return des3.encrypt(des2.decrypt(des1.encrypt(bdata)));
  endfunction
  function uBlockT decrypt(uBlockT bdata);
    keyDispatch();
    return des1.decrypt(des2.encrypt(des3.decrypt(bdata)));
  endfunction
endclass : CoreTDEA
