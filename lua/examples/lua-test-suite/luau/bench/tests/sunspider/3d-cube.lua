-- 3D Cube Rotation
-- http://www.speich.net/computer/moztesting/3d.htm
-- Created by Simon Speich

local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local Q = {}
local MTrans = {};  -- transformation matrix
local MQube = {}  -- position information of qube
local I = {}      -- entity matrix
local Origin = {}
local Testing = {}
local LoopTimer;

local validation = {
 [20] = 2889,
 [40] = 2889,
 [80] = 2889,
 [160] = 2889
};

local DisplArea = {}
DisplArea.Width = 300;
DisplArea.Height = 300;

local function DrawLine(From, To)
  local x1 = From.V[1];
  local x2 = To.V[1];
  local y1 = From.V[2];
  local y2 = To.V[2];
  local dx = math.abs(x2 - x1);
  local dy = math.abs(y2 - y1);
  local x = x1;
  local y = y1;
  local IncX1, IncY1;
  local IncX2, IncY2;  
  local Den;
  local Num;
  local NumAdd;
  local NumPix;

  if (x2 >= x1) then  IncX1 = 1; IncX2 = 1;
  else IncX1 = -1; IncX2 = -1; end

  if (y2 >= y1)  then  IncY1 = 1; IncY2 = 1;
  else IncY1 = -1; IncY2 = -1; end

  if (dx >= dy) then
    IncX1 = 0;
    IncY2 = 0;
    Den = dx;
    Num = dx / 2;
    NumAdd = dy;
    NumPix = dx;
  else 
    IncX2 = 0;
    IncY1 = 0;
    Den = dy;
    Num = dy / 2;
    NumAdd = dx;
    NumPix = dy;
  end

  NumPix = math.floor(Q.LastPx + NumPix + 0.5);

  local i = Q.LastPx;
  while i < NumPix do
    Num = Num + NumAdd;
    if (Num >= Den) then
      Num = Num - Den;
      x = x + IncX1;
      y = y + IncY1;
    end
    x = x + IncX2;
    y = y + IncY2;

    i = i + 1;
  end
  Q.LastPx = NumPix;
end

local function CalcCross(V0, V1)
  local Cross = {};
  Cross[1] = V0[2]*V1[3] - V0[3]*V1[2];
  Cross[2] = V0[3]*V1[1] - V0[1]*V1[3];
  Cross[3] = V0[1]*V1[2] - V0[2]*V1[1];
  return Cross;
end

local function CalcNormal(V0, V1, V2)
  local A = {};   local B = {}; 
  for i = 1,3 do
    A[i] = V0[i] - V1[i];
    B[i] = V2[i] - V1[i];
  end
  A = CalcCross(A, B);
  local Length = math.sqrt(A[1]*A[1] + A[2]*A[2] + A[3]*A[3]); 
  for i = 1,3 do A[i] = A[i] / Length; end
  A[4] = 1;
  return A;
end

local function CreateP(X,Y,Z)
  local result = {}
  result.V = {X,Y,Z,1};
  return result
end

-- multiplies two matrices
local function MMulti(M1, M2)
  local M = {{},{},{},{}};
  for i = 1,4 do
    for j = 1,4 do
      M[i][j] = M1[i][1] * M2[1][j] + M1[i][2] * M2[2][j] + M1[i][3] * M2[3][j] + M1[i][4] * M2[4][j];
    end
  end
  return M;
end

-- multiplies matrix with vector
local function VMulti(M, V)
  local Vect = {};
  for i = 1,4 do
    Vect[i] = M[i][1] * V[1] + M[i][2] * V[2] + M[i][3] * V[3] + M[i][4] * V[4];
  end
  return Vect;
end

local function VMulti2(M, V)
  local Vect = {};
  for i = 1,3 do
    Vect[i] = M[i][1] * V[1] + M[i][2] * V[2] + M[i][3] * V[3];
  end
  return Vect;
end

-- add to matrices
local function MAdd(M1, M2)
  local M = {{},{},{},{}};
  for i = 1,4 do
    for j = 1,4 do
      M[i][j] = M1[i][j] + M2[i][j];
    end
  end
  return M;
end

local function Translate(M, Dx, Dy, Dz)
    local T = {
  {1,0,0,Dx},
  {0,1,0,Dy},
  {0,0,1,Dz},
  {0,0,0,1}
    };
  return MMulti(T, M);
end

local function RotateX(M, Phi)
    local a = Phi;
  a = a * math.pi / 180;
  local Cos = math.cos(a);
  local Sin = math.sin(a);
  local R = {
  {1,0,0,0},
  {0,Cos,-Sin,0},
  {0,Sin,Cos,0},
  {0,0,0,1}
  };
  return MMulti(R, M);
end

local function RotateY(M, Phi)
    local a = Phi;
  a = a * math.pi / 180;
  local Cos = math.cos(a);
  local Sin = math.sin(a);
  local R = {
  {Cos,0,Sin,0},
  {0,1,0,0},
  {-Sin,0,Cos,0},
  {0,0,0,1}
  };
  return MMulti(R, M);
end

local function RotateZ(M, Phi)
    local a = Phi;
  a = a * math.pi / 180;
  local Cos = math.cos(a);
  local Sin = math.sin(a);
  local R = {
  {Cos,-Sin,0,0},
  {Sin,Cos,0,0},
  {0,0,1,0},   
  {0,0,0,1}
  };
  return MMulti(R, M);
end

local function DrawQube()
  -- calc current normals
  local CurN = {};
  local i = 5;
  Q.LastPx = 0;
  while i > -1 do CurN[i+1] = VMulti2(MQube, Q.Normal[i+1]); i = i - 1 end
  if (CurN[1][3] < 0) then
    if (not Q.Line[1]) then DrawLine(Q[1], Q[2]); Q.Line[1] = true; end
    if (not Q.Line[2]) then DrawLine(Q[2], Q[3]); Q.Line[2] = true; end
    if (not Q.Line[3]) then DrawLine(Q[3], Q[4]); Q.Line[3] = true; end
    if (not Q.Line[4]) then DrawLine(Q[4], Q[1]); Q.Line[4] = true; end
  end
  if (CurN[2][3] < 0) then
    if (not Q.Line[3]) then DrawLine(Q[4], Q[3]); Q.Line[3] = true; end
    if (not Q.Line[10]) then DrawLine(Q[3], Q[7]); Q.Line[10] = true; end
    if (not Q.Line[7]) then DrawLine(Q[7], Q[8]); Q.Line[7] = true; end
    if (not Q.Line[11]) then DrawLine(Q[8], Q[4]); Q.Line[11] = true; end
  end
  if (CurN[3][3] < 0) then
    if (not Q.Line[5]) then DrawLine(Q[5], Q[6]); Q.Line[6] = true; end
    if (not Q.Line[6]) then DrawLine(Q[6], Q[7]); Q.Line[6] = true; end
    if (not Q.Line[7]) then DrawLine(Q[7], Q[8]); Q.Line[7] = true; end
    if (not Q.Line[8]) then DrawLine(Q[8], Q[5]); Q.Line[8] = true; end
  end
  if (CurN[4][3] < 0) then
    if (not Q.Line[5]) then DrawLine(Q[5], Q[6]); Q.Line[5] = true; end
    if (not Q.Line[9]) then DrawLine(Q[6], Q[2]); Q.Line[9] = true; end
    if (not Q.Line[1]) then DrawLine(Q[2], Q[1]); Q.Line[1] = true; end
    if (not Q.Line[12]) then DrawLine(Q[1], Q[5]); Q.Line[12] = true; end
  end
  if (CurN[5][3] < 0) then
    if (not Q.Line[12]) then DrawLine(Q[5], Q[1]); Q.Line[12] = true; end
    if (not Q.Line[4]) then DrawLine(Q[1], Q[4]); Q.Line[4] = true; end
    if (not Q.Line[11]) then DrawLine(Q[4], Q[8]); Q.Line[11] = true; end
    if (not Q.Line[8]) then DrawLine(Q[8], Q[5]); Q.Line[8] = true; end
  end
  if (CurN[6][3] < 0) then
    if (not Q.Line[9]) then DrawLine(Q[2], Q[6]); Q.Line[9] = true; end
    if (not Q.Line[6]) then DrawLine(Q[6], Q[7]); Q.Line[6] = true; end
    if (not Q.Line[10]) then DrawLine(Q[7], Q[3]); Q.Line[10] = true; end
    if (not Q.Line[2]) then DrawLine(Q[3], Q[2]); Q.Line[2] = true; end
  end
  Q.Line = {false,false,false,false,false,false,false,false,false,false,false,false}
  Q.LastPx = 0;
end

local function Loop()
  if (Testing.LoopCount > Testing.LoopMax) then return; end
  local TestingStr = tostring(Testing.LoopCount);
  while (#TestingStr < 3) do TestingStr = "0" .. TestingStr; end
  MTrans = Translate(I, -Q[9].V[1], -Q[9].V[2], -Q[9].V[3]);
  MTrans = RotateX(MTrans, 1);
  MTrans = RotateY(MTrans, 3);
  MTrans = RotateZ(MTrans, 5);
  MTrans = Translate(MTrans, Q[9].V[1], Q[9].V[2], Q[9].V[3]);
  MQube = MMulti(MTrans, MQube);
  local i = 8;
  while i > -1 do
    Q[i+1].V = VMulti(MTrans, Q[i+1].V);
    i = i - 1
  end
  DrawQube();
  Testing.LoopCount = Testing.LoopCount + 1;
  Loop();
end

local function Init(CubeSize)
  -- init/reset vars
  Origin.V = {150,150,20,1};
  Testing.LoopCount = 0;
  Testing.LoopMax = 50;
  Testing.TimeMax = 0;
  Testing.TimeAvg = 0;
  Testing.TimeMin = 0;
  Testing.TimeTemp = 0;
  Testing.TimeTotal = 0;
  Testing.Init = false;

  -- transformation matrix
  MTrans = {
  {1,0,0,0},
  {0,1,0,0},
  {0,0,1,0},
  {0,0,0,1}
  };
  
  -- position information of qube
  MQube = {
  {1,0,0,0},
  {0,1,0,0},
  {0,0,1,0},
  {0,0,0,1}
  };
  
  -- entity matrix
  I = {
  {1,0,0,0},
  {0,1,0,0},
  {0,0,1,0},
  {0,0,0,1}
  };
  
  -- create qube
  Q[1] = CreateP(-CubeSize,-CubeSize, CubeSize);
  Q[2] = CreateP(-CubeSize, CubeSize, CubeSize);
  Q[3] = CreateP( CubeSize, CubeSize, CubeSize);
  Q[4] = CreateP( CubeSize,-CubeSize, CubeSize);
  Q[5] = CreateP(-CubeSize,-CubeSize,-CubeSize);
  Q[6] = CreateP(-CubeSize, CubeSize,-CubeSize);
  Q[7] = CreateP( CubeSize, CubeSize,-CubeSize);
  Q[8] = CreateP( CubeSize,-CubeSize,-CubeSize);
  
  -- center of gravity
  Q[9] = CreateP(0, 0, 0);
  
  -- anti-clockwise edge check
  Q.Edge = {{1,2,3},{4,5,7},{8,7,6},{5,6,2},{5,1,4},{2,6,7}};
  
  -- calculate squad normals
  Q.Normal = {};
  for i = 1,#Q.Edge do
    Q.Normal[i] = CalcNormal(Q[Q.Edge[i][1]].V, Q[Q.Edge[i][2]].V, Q[Q.Edge[i][3]].V);
  end
  
  -- line drawn ?
  Q.Line = {false,false,false,false,false,false,false,false,false,false,false,false};
  
  -- create line pixels
  Q.NumPx = 9 * 2 * CubeSize;
  for i = 1,Q.NumPx do CreateP(0,0,0); end
  
  MTrans = Translate(MTrans, Origin.V[1], Origin.V[2], Origin.V[3]);
  MQube = MMulti(MTrans, MQube);

  local i = 0;
  while i < 9 do
    Q[i+1].V = VMulti(MTrans, Q[i+1].V);
    i = i + 1
  end
  DrawQube();
  Testing.Init = true;
  Loop();
  
  -- Perform a simple sum-based verification.
  local sum = 0;
  for i = 1,#Q do
    local vector = Q[i].V;
    for j = 1,#vector do
      sum = sum + vector[j];
    end
  end
  if (math.floor(sum) ~= validation[CubeSize]) then
    assert(false, "Error: bad vector sum for CubeSize = " .. CubeSize .. "; expected " .. validation[CubeSize] .. " but got " .. math.floor(sum))
  end
end

local i = 20
while i <= 160 do
  Init(i);
  i = i * 2
end

Q = nil;
MTrans = nil;
MQube = nil;
I = nil;
Origin = nil;
Testing = nil;
LoopTime = nil;
DisplArea = nil;

end

bench.runCode(test, "3d-cube")
