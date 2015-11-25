model Capacitor
  parameter Capacitance C;
  Voltage u "Voltage drop between pin_p and pin_n";
  Pin pin_p, pin_n;
equation
  0 = pin_p.i + pin_n.i;
  u = pin_p.v - pin_n.v;
  C * der(u) = pin_p.i;
end Capacitor;
