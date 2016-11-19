model FirstOrder
  parameter Real c=1 "Time constant";
  Real x "An unknown";
equation
  der(x) = -c*x "A first order differential equation";
end FirstOrder;

