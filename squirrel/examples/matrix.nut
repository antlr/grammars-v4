/*
*
* Original Javascript version by David Hedbor(http://www.bagley.org/~doug/shootout/)
*
*/
local SIZE=30;

function mkmatrix(rows, cols) {
  local i, j, count = 1;
  local m = []; m.resize(rows);
  for (i = 0; i < rows; i+=1) {
    m[i] = [];m[i].resize(cols)
    for (j = 0; j < cols; j+=1) {
      m[i][j] = count+=1;
    }
  }
  return m;
}

function mmult(rows, cols,  m1, m2, m3) {
  local i, j, k, val;
  for (i = 0; i < rows; i+=1) {
    for (j = 0; j < cols; j+=1) {
      val = 0;
      for (k = 0; k < cols; k+=1) {
    val += m1[i][k] * m2[k][j];
      }
      m3[i][j] = val;
    }
  }
  return m3;
}

local n = vargv.len()!=0?vargv[0].tointeger():1

local m1 = mkmatrix(SIZE, SIZE);
local m2 = mkmatrix(SIZE, SIZE);
local mm = mkmatrix(SIZE, SIZE);

for (local i = 0; i < n; i+=1) {
  mmult(SIZE, SIZE, m1, m2, mm);
}

print(mm[0][0]+" "+mm[2][3]+" "+mm[3][2]+" "+mm[4][4]);
