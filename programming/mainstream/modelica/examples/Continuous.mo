within Modelica.Blocks;
package Continuous "Library of continuous control blocks with internal states"

  import Modelica.Blocks.Interfaces;
  import Modelica.SIunits;
  extends Modelica.Icons.Package;

  block Integrator "Output the integral of the input signal"
    import Modelica.Blocks.Types.Init;
    parameter Real k(unit="1")=1 "Integrator gain";

    /* InitialState is the default, because it was the default in Modelica 2.2
     and therefore this setting is backward compatible
  */
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.InitialState
      "Type of initialization (1: no init, 2: steady state, 3,4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real y_start=0 "Initial or guess value of output (= state)"
      annotation (Dialog(group="Initialization"));
    extends Interfaces.SISO(y(start=y_start));

  initial equation
    if initType == Init.SteadyState then
       der(y) = 0;
    elseif initType == Init.InitialState or
           initType == Init.InitialOutput then
      y = y_start;
    end if;
  equation
    der(y) = k*u;
    annotation (
      Documentation(info="<html>
<p>
This blocks computes output <b>y</b> (element-wise) as
<i>integral</i> of the input <b>u</b> multiplied with
the gain <i>k</i>:
</p>
<pre>
         k
     y = - u
         s
</pre>

<p>
It might be difficult to initialize the integrator in steady state.
This is discussed in the description of package
<a href=\"modelica://Modelica.Blocks.Continuous#info\">Continuous</a>.
</p>

</html>"),   Icon(coordinateSystem(
            preserveAspectRatio=true,
            extent={{-100.0,-100.0},{100.0,100.0}}),
          graphics={
            Line(
              points={{-80.0,78.0},{-80.0,-90.0}},
              color={192,192,192}),
            Polygon(
              lineColor={192,192,192},
              fillColor={192,192,192},
              fillPattern=FillPattern.Solid,
              points={{-80.0,90.0},{-88.0,68.0},{-72.0,68.0},{-80.0,90.0}}),
            Line(
              points={{-90.0,-80.0},{82.0,-80.0}},
              color={192,192,192}),
            Polygon(
              lineColor={192,192,192},
              fillColor={192,192,192},
              fillPattern=FillPattern.Solid,
              points={{90.0,-80.0},{68.0,-72.0},{68.0,-88.0},{90.0,-80.0}}),
            Text(
              lineColor={192,192,192},
              extent={{0.0,-70.0},{60.0,-10.0}},
              textString="I"),
            Text(
              extent={{-150.0,-150.0},{150.0,-110.0}},
              textString="k=%k"),
            Line(
              points={{-80.0,-80.0},{80.0,80.0}},
              color={0,0,127})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255}),
          Text(
            extent={{-36,60},{32,2}},
            lineColor={0,0,0},
            textString="k"),
          Text(
            extent={{-32,0},{36,-58}},
            lineColor={0,0,0},
            textString="s"),
          Line(points={{-46,0},{46,0}})}));
  end Integrator;

  block LimIntegrator "Integrator with limited value of the output"
    import Modelica.Blocks.Types.Init;
    parameter Real k(unit="1")=1 "Integrator gain";
    parameter Real outMax(start=1) "Upper limit of output";
    parameter Real outMin=-outMax "Lower limit of output";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.InitialState
      "Type of initialization (1: no init, 2: steady state, 3/4: initial output)"
      annotation(Evaluate=true, Dialog(group="Initialization"));
    parameter Boolean limitsAtInit = true
      "= false, if limits are ignored during initialization (i.e., der(y)=k*u)"
      annotation(Evaluate=true, Dialog(group="Initialization"));
    parameter Real y_start=0
      "Initial or guess value of output (must be in the limits outMin .. outMax)"
      annotation (Dialog(group="Initialization"));
    parameter Boolean strict=false "= true, if strict limits with noEvent(..)"
      annotation (Evaluate=true, choices(checkBox=true), Dialog(tab="Advanced"));
    extends Interfaces.SISO(y(start=y_start));

  initial equation
    if initType == Init.SteadyState then
       der(y) = 0;
    elseif initType == Init.InitialState or
           initType == Init.InitialOutput then
      y = y_start;
    end if;
  equation
    if initial() and not limitsAtInit then
       der(y) = k*u;
       assert(y >= outMin - 0.001*abs(outMax-outMin) and y <= outMax + 0.001*abs(outMax-outMin),
            "LimIntegrator: During initialization the limits have been ignored.\n"
          + "However, the result is that the output y is not within the required limits:\n"
          + "  y = " + String(y) + ", outMin = " + String(outMin) + ", outMax = " + String(outMax));
    elseif strict then
       der(y) = noEvent(if y < outMin and k*u < 0 or y > outMax and k*u > 0 then 0 else k*u);
    else
       der(y) = if y < outMin and k*u < 0 or y > outMax and k*u > 0 then 0 else k*u;
    end if;
    annotation (
      Documentation(info="<html>
<p>
This blocks computes <b>y</b> (element-wise) as <i>integral</i>
of the input <b>u</b> multiplied with the gain <i>k</i>. If the
integral reaches a given upper or lower <i>limit</i> and the
input will drive the integral outside of this bound, the
integration is halted and only restarted if the input drives
the integral away from the bounds.
</p>

<p>
It might be difficult to initialize the integrator in steady state.
This is discussed in the description of package
<a href=\"modelica://Modelica.Blocks.Continuous#info\">Continuous</a>.
</p>

<p>
If parameter <b>limitAtInit</b> = <b>false</b>, the limits of the
integrator are removed from the initialization problem which
leads to a much simpler equation system. After initialization has been
performed, it is checked via an assert whether the output is in the
defined limits. For backward compatibility reasons
<b>limitAtInit</b> = <b>true</b>. In most cases it is best
to use <b>limitAtInit</b> = <b>false</b>.
</p>
</html>"),   Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Line(points={{-80,78},{-80,-90}}, color={192,192,192}),
          Polygon(
            points={{-80,90},{-88,68},{-72,68},{-80,90}},
            lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,-80},{82,-80}}, color={192,192,192}),
          Polygon(
            points={{90,-80},{68,-72},{68,-88},{90,-80}},
            lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid),
          Line(points={{-80,-80},{20,20},{80,20}}, color={0,0,127}),
          Text(
            extent={{0,-10},{60,-70}},
            lineColor={192,192,192},
            textString="I"),
          Text(
            extent={{-150,-150},{150,-110}},
            lineColor={0,0,0},
            textString="k=%k"),
          Line(
            visible=strict,
            points={{20,20},{80,20}},
            color={255,0,0})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Text(
            extent={{-54,46},{-4,-48}},
            lineColor={0,0,0},
            textString="lim"),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255}),
          Text(
            extent={{-8,60},{60,2}},
            lineColor={0,0,0},
            textString="k"),
          Text(
            extent={{-8,-2},{60,-60}},
            lineColor={0,0,0},
            textString="s"),
          Line(points={{4,0},{46,0}})}));
  end LimIntegrator;

  block Derivative "Approximated derivative block"
    import Modelica.Blocks.Types.Init;
    parameter Real k(unit="1")=1 "Gains";
    parameter SIunits.Time T(min=Modelica.Constants.small) = 0.01
      "Time constants (T>0 required; T=0 is ideal derivative block)";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real x_start=0 "Initial or guess value of state"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start=0 "Initial value of output (= state)"
      annotation(Dialog(enable=initType == Init.InitialOutput, group=
            "Initialization"));
    extends Interfaces.SISO;

    output Real x(start=x_start) "State of block";

  protected
    parameter Boolean zeroGain = abs(k) < Modelica.Constants.eps;
  initial equation
    if initType == Init.SteadyState then
      der(x) = 0;
    elseif initType == Init.InitialState then
      x = x_start;
    elseif initType == Init.InitialOutput then
      if zeroGain then
         x = u;
      else
         y = y_start;
      end if;
    end if;
  equation
    der(x) = if zeroGain then 0 else (u - x)/T;
    y = if zeroGain then 0 else (k/T)*(u - x);
    annotation (
      Documentation(info="<html>
<p>
This blocks defines the transfer function between the
input u and the output y
(element-wise) as <i>approximated derivative</i>:
</p>
<pre>
             k * s
     y = ------------ * u
            T * s + 1
</pre>
<p>
If you would like to be able to change easily between different
transfer functions (FirstOrder, SecondOrder, ... ) by changing
parameters, use the general block <b>TransferFunction</b> instead
and model a derivative block with parameters<br>
b = {k,0}, a = {T, 1}.
</p>

<p>
If k=0, the block reduces to y=0.
</p>
</html>"),   Icon(
      coordinateSystem(preserveAspectRatio=true,
          extent={{-100.0,-100.0},{100.0,100.0}}),
        graphics={
      Line(points={{-80.0,78.0},{-80.0,-90.0}},
        color={192,192,192}),
    Polygon(lineColor={192,192,192},
      fillColor={192,192,192},
      fillPattern=FillPattern.Solid,
      points={{-80.0,90.0},{-88.0,68.0},{-72.0,68.0},{-80.0,90.0}}),
    Line(points={{-90.0,-80.0},{82.0,-80.0}},
      color={192,192,192}),
    Polygon(lineColor={192,192,192},
      fillColor={192,192,192},
      fillPattern=FillPattern.Solid,
      points={{90.0,-80.0},{68.0,-72.0},{68.0,-88.0},{90.0,-80.0}}),
    Line(origin = {-24.667,-27.333},
      points = {{-55.333,87.333},{-19.333,-40.667},{86.667,-52.667}},
      color = {0,0,127},
      smooth = Smooth.Bezier),
    Text(lineColor={192,192,192},
      extent={{-30.0,14.0},{86.0,60.0}},
      textString="DT1"),
    Text(extent={{-150.0,-150.0},{150.0,-110.0}},
      textString="k=%k")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Text(
            extent={{-54,52},{50,10}},
            lineColor={0,0,0},
            textString="k s"),
          Text(
            extent={{-54,-6},{52,-52}},
            lineColor={0,0,0},
            textString="T s + 1"),
          Line(points={{-50,0},{50,0}}),
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255})}));
  end Derivative;

  block FirstOrder "First order transfer function block (= 1 pole)"
    import Modelica.Blocks.Types.Init;
    parameter Real k(unit="1")=1 "Gain";
    parameter SIunits.Time T(start=1) "Time Constant";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3/4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real y_start=0 "Initial or guess value of output (= state)"
      annotation (Dialog(group="Initialization"));

    extends Interfaces.SISO(y(start=y_start));

  initial equation
    if initType == Init.SteadyState then
      der(y) = 0;
    elseif initType == Init.InitialState or initType == Init.InitialOutput then
      y = y_start;
    end if;
  equation
    der(y) = (k*u - y)/T;
    annotation (
      Documentation(info="<html>
<p>
This blocks defines the transfer function between the input u
and the output y (element-wise) as <i>first order</i> system:
</p>
<pre>
               k
     y = ------------ * u
            T * s + 1
</pre>
<p>
If you would like to be able to change easily between different
transfer functions (FirstOrder, SecondOrder, ... ) by changing
parameters, use the general block <b>TransferFunction</b> instead
and model a first order SISO system with parameters<br>
b = {k}, a = {T, 1}.
</p>
<pre>
Example:
   parameter: k = 0.3, T = 0.4
   results in:
             0.3
      y = ----------- * u
          0.4 s + 1.0
</pre>

</html>"),   Icon(
    coordinateSystem(preserveAspectRatio=true,
        extent={{-100.0,-100.0},{100.0,100.0}}),
      graphics={
    Line(points={{-80.0,78.0},{-80.0,-90.0}},
      color={192,192,192}),
    Polygon(lineColor={192,192,192},
      fillColor={192,192,192},
      fillPattern=FillPattern.Solid,
      points={{-80.0,90.0},{-88.0,68.0},{-72.0,68.0},{-80.0,90.0}}),
    Line(points={{-90.0,-80.0},{82.0,-80.0}},
      color={192,192,192}),
    Polygon(lineColor={192,192,192},
      fillColor={192,192,192},
      fillPattern=FillPattern.Solid,
      points={{90.0,-80.0},{68.0,-72.0},{68.0,-88.0},{90.0,-80.0}}),
    Line(origin = {-26.667,6.667},
        points = {{106.667,43.333},{-13.333,29.333},{-53.333,-86.667}},
        color = {0,0,127},
        smooth = Smooth.Bezier),
    Text(lineColor={192,192,192},
      extent={{0.0,-60.0},{60.0,0.0}},
      textString="PT1"),
    Text(extent={{-150.0,-150.0},{150.0,-110.0}},
      textString="T=%T")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Text(
            extent={{-48,52},{50,8}},
            lineColor={0,0,0},
            textString="k"),
          Text(
            extent={{-54,-6},{56,-56}},
            lineColor={0,0,0},
            textString="T s + 1"),
          Line(points={{-50,0},{50,0}}),
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255})}));
  end FirstOrder;

  block SecondOrder "Second order transfer function block (= 2 poles)"
    import Modelica.Blocks.Types.Init;
    parameter Real k(unit="1")=1 "Gain";
    parameter Real w(start=1) "Angular frequency";
    parameter Real D(start=1) "Damping";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3/4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real y_start=0 "Initial or guess value of output (= state)"
      annotation (Dialog(group="Initialization"));
    parameter Real yd_start=0
      "Initial or guess value of derivative of output (= state)"
      annotation (Dialog(group="Initialization"));

    extends Interfaces.SISO(y(start=y_start));
    output Real yd(start=yd_start) "Derivative of y";

  initial equation
    if initType == Init.SteadyState then
      der(y) = 0;
      der(yd) = 0;
    elseif initType == Init.InitialState or initType == Init.InitialOutput then
      y = y_start;
      yd = yd_start;
    end if;
  equation
    der(y) = yd;
    der(yd) = w*(w*(k*u - y) - 2*D*yd);
    annotation (
      Documentation(info="<html>
<p>
This blocks defines the transfer function between the input u and
the output y (element-wise) as <i>second order</i> system:
</p>
<pre>
                             k
     y = ---------------------------------------- * u
            ( s / w )^2 + 2*D*( s / w ) + 1
</pre>
<p>
If you would like to be able to change easily between different
transfer functions (FirstOrder, SecondOrder, ... ) by changing
parameters, use the general model class <b>TransferFunction</b>
instead and model a second order SISO system with parameters<br>
b = {k}, a = {1/w^2, 2*D/w, 1}.
</p>
<pre>
Example:

   parameter: k =  0.3,  w = 0.5,  D = 0.4
   results in:
                  0.3
      y = ------------------- * u
          4.0 s^2 + 1.6 s + 1
</pre>

</html>"),   Icon(
        coordinateSystem(preserveAspectRatio=true,
              extent={{-100.0,-100.0},{100.0,100.0}}),
            graphics={
        Line(points={{-80.0,78.0},{-80.0,-90.0}},
            color={192,192,192}),
      Polygon(lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid,
          points={{-80.0,90.0},{-88.0,68.0},{-72.0,68.0},{-80.0,90.0}}),
      Line(points={{-90.0,-80.0},{82.0,-80.0}},
          color={192,192,192}),
      Polygon(lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid,
          points={{90.0,-80.0},{68.0,-72.0},{68.0,-88.0},{90.0,-80.0}}),
      Line(origin = {-1.939,-1.816},
          points = {{81.939,36.056},{65.362,36.056},{14.39,-26.199},{-29.966,113.485},{-65.374,-61.217},{-78.061,-78.184}},
          color = {0,0,127},
          smooth = Smooth.Bezier),
      Text(lineColor={192,192,192},
          extent={{0.0,-70.0},{60.0,-10.0}},
          textString="PT2"),
      Text(extent={{-150.0,-150.0},{150.0,-110.0}},
          textString="w=%w")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Text(
            extent={{-60,60},{60,14}},
            lineColor={0,0,0},
            textString="k"),
          Text(
            extent={{-60,8},{-32,-20}},
            lineColor={0,0,0},
            textString="s"),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255}),
          Line(points={{-50,14},{50,14}}),
          Line(points={{-54,-20},{-38,-20}}),
          Text(
            extent={{-52,-26},{-36,-48}},
            lineColor={0,0,0},
            textString="w"),
          Line(points={{-50,2},{-56,-8},{-56,-28},{-52,-46}}),
          Line(points={{-40,2},{-34,-10},{-34,-30},{-38,-46}}),
          Text(
            extent={{-34,8},{-22,-10}},
            lineColor={0,0,0},
            textString="2"),
          Text(
            extent={{-34,-6},{6,-36}},
            lineColor={0,0,0},
            textString="+2D"),
          Text(
            extent={{2,8},{30,-20}},
            lineColor={0,0,0},
            textString="s"),
          Line(points={{8,-20},{24,-20}}),
          Text(
            extent={{10,-26},{26,-48}},
            lineColor={0,0,0},
            textString="w"),
          Line(points={{12,2},{6,-8},{6,-28},{10,-46}}),
          Line(points={{22,2},{28,-10},{28,-30},{24,-46}}),
          Text(
            extent={{30,2},{58,-42}},
            lineColor={0,0,0},
            textString="+1")}));
  end SecondOrder;

  block PI "Proportional-Integral controller"
    import Modelica.Blocks.Types.Init;
    parameter Real k(unit="1")=1 "Gain";
    parameter SIunits.Time T(start=1,min=Modelica.Constants.small)
      "Time Constant (T>0 required)";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                                                              annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real x_start=0 "Initial or guess value of state"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start=0 "Initial value of output"
      annotation(Dialog(enable=initType == Init.SteadyState or initType == Init.InitialOutput, group=
            "Initialization"));

    extends Interfaces.SISO;
    output Real x(start=x_start) "State of block";

  initial equation
    if initType == Init.SteadyState then
      der(x) = 0;
    elseif initType == Init.InitialState then
      x = x_start;
    elseif initType == Init.InitialOutput then
      y = y_start;
    end if;
  equation
    der(x) = u/T;
    y = k*(x + u);
    annotation (defaultComponentName="PI",
      Documentation(info="<html>
<p>
This blocks defines the transfer function between the input u and
the output y (element-wise) as <i>PI</i> system:
</p>
<pre>
                 1
   y = k * (1 + ---) * u
                T*s
           T*s + 1
     = k * ------- * u
             T*s
</pre>
<p>
If you would like to be able to change easily between different
transfer functions (FirstOrder, SecondOrder, ... ) by changing
parameters, use the general model class <b>TransferFunction</b>
instead and model a PI SISO system with parameters<br>
b = {k*T, k}, a = {T, 0}.
</p>
<pre>
Example:

   parameter: k = 0.3,  T = 0.4

   results in:
               0.4 s + 1
      y = 0.3 ----------- * u
                 0.4 s
</pre>

<p>
It might be difficult to initialize the PI component in steady state
due to the integrator part.
This is discussed in the description of package
<a href=\"modelica://Modelica.Blocks.Continuous#info\">Continuous</a>.
</p>

</html>"),   Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Line(points={{-80,78},{-80,-90}}, color={192,192,192}),
          Polygon(
            points={{-80,90},{-88,68},{-72,68},{-80,90}},
            lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,-80},{82,-80}}, color={192,192,192}),
          Polygon(
            points={{90,-80},{68,-72},{68,-88},{90,-80}},
            lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid),
          Line(points = {{-80.0,-80.0},{-80.0,-20.0},{60.0,80.0}}, color = {0,0,127}),
          Text(
            extent={{0,6},{60,-56}},
            lineColor={192,192,192},
            textString="PI"),
          Text(
            extent={{-150,-150},{150,-110}},
            lineColor={0,0,0},
            textString="T=%T")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Text(
            extent={{-68,24},{-24,-18}},
            lineColor={0,0,0},
            textString="k"),
          Text(
            extent={{-32,48},{60,0}},
            lineColor={0,0,0},
            textString="T s + 1"),
          Text(
            extent={{-30,-8},{52,-40}},
            lineColor={0,0,0},
            textString="T s"),
          Line(points={{-24,0},{54,0}}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{62,0},{100,0}}, color={0,0,255})}));
  end PI;

  block PID "PID-controller in additive description form"
    import Modelica.Blocks.Types.InitPID;
    import Modelica.Blocks.Types.Init;
    extends Interfaces.SISO;

    parameter Real k(unit="1")=1 "Gain";
    parameter SIunits.Time Ti(min=Modelica.Constants.small, start=0.5)
      "Time Constant of Integrator";
    parameter SIunits.Time Td(min=0, start=0.1)
      "Time Constant of Derivative block";
    parameter Real Nd(min=Modelica.Constants.small) = 10
      "The higher Nd, the more ideal the derivative block";
    parameter Modelica.Blocks.Types.InitPID initType= Modelica.Blocks.Types.InitPID.DoNotUse_InitialIntegratorState
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                       annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real xi_start=0
      "Initial or guess value value for integrator output (= integrator state)"
      annotation (Dialog(group="Initialization"));
    parameter Real xd_start=0
      "Initial or guess value for state of derivative block"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start=0 "Initial value of output"
      annotation(Dialog(enable=initType == InitPID.InitialOutput, group=
            "Initialization"));
    constant SI.Time unitTime=1  annotation(HideResult=true);

    Blocks.Math.Gain P(k=1) "Proportional part of PID controller"
      annotation (Placement(transformation(extent={{-60,60},{-20,100}})));
    Blocks.Continuous.Integrator I(k=unitTime/Ti, y_start=xi_start,
      initType=if initType==InitPID.SteadyState then
                  Init.SteadyState else
               if initType==InitPID.InitialState or
                  initType==InitPID.DoNotUse_InitialIntegratorState then
                  Init.InitialState else Init.NoInit)
      "Integral part of PID controller"
      annotation (Placement(transformation(extent={{-60,-20},{-20,20}})));
    Blocks.Continuous.Derivative D(k=Td/unitTime, T=max([Td/Nd, 100*Modelica.
          Constants.eps]), x_start=xd_start,
      initType=if initType==InitPID.SteadyState or
                  initType==InitPID.InitialOutput then Init.SteadyState else
               if initType==InitPID.InitialState then Init.InitialState else
                  Init.NoInit) "Derivative part of PID controller"
      annotation (Placement(transformation(extent={{-60,-100},{-20,-60}})));
    Blocks.Math.Gain Gain(k=k) "Gain of PID controller"
      annotation (Placement(transformation(extent={{60,-10},{80,10}})));
    Blocks.Math.Add3 Add annotation (Placement(transformation(extent={{20,-10},
              {40,10}})));
  initial equation
    if initType==InitPID.InitialOutput then
       y = y_start;
    end if;

  equation
    connect(u, P.u) annotation (Line(points={{-120,0},{-80,0},{-80,80},{-64,80}},
          color={0,0,127}));
    connect(u, I.u)
      annotation (Line(points={{-120,0},{-64,0}}, color={0,0,127}));
    connect(u, D.u) annotation (Line(points={{-120,0},{-80,0},{-80,-80},{-64,
            -80}}, color={0,0,127}));
    connect(P.y, Add.u1) annotation (Line(points={{-18,80},{0,80},{0,8},{18,8}},
          color={0,0,127}));
    connect(I.y, Add.u2)
      annotation (Line(points={{-18,0},{18,0}}, color={0,0,127}));
    connect(D.y, Add.u3) annotation (Line(points={{-18,-80},{0,-80},{0,-8},{18,
            -8}}, color={0,0,127}));
    connect(Add.y, Gain.u)
      annotation (Line(points={{41,0},{58,0}}, color={0,0,127}));
    connect(Gain.y, y)
      annotation (Line(points={{81,0},{110,0}}, color={0,0,127}));
    annotation (defaultComponentName="PID",
      Icon(
          coordinateSystem(preserveAspectRatio=true,
              extent={{-100.0,-100.0},{100.0,100.0}}),
              graphics={
          Line(points={{-80.0,78.0},{-80.0,-90.0}},
              color={192,192,192}),
        Polygon(lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid,
            points={{-80.0,90.0},{-88.0,68.0},{-72.0,68.0},{-80.0,90.0}}),
        Line(points={{-90.0,-80.0},{82.0,-80.0}},
            color={192,192,192}),
        Polygon(lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid,
            points={{90.0,-80.0},{68.0,-72.0},{68.0,-88.0},{90.0,-80.0}}),
        Line(points = {{-80,-80},{-80,-20},{60,80}}, color = {0,0,127}),
        Text(lineColor={192,192,192},
            extent={{-20.0,-60.0},{80.0,-20.0}},
            textString="PID"),
        Text(extent={{-150.0,-150.0},{150.0,-110.0}},
            textString="Ti=%Ti")}),
      Documentation(info="<html>
<p>
This is the text-book version of a PID-controller.
For a more practically useful PID-controller, use
block LimPID.
</p>

<p>
The PID block can be initialized in different
ways controlled by parameter <b>initType</b>. The possible
values of initType are defined in
<a href=\"modelica://Modelica.Blocks.Types.InitPID\">Modelica.Blocks.Types.InitPID</a>.
This type is identical to
<a href=\"modelica://Modelica.Blocks.Types.Init\">Types.Init</a>,
with the only exception that the additional option
<b>DoNotUse_InitialIntegratorState</b> is added for
backward compatibility reasons (= integrator is initialized with
InitialState whereas differential part is initialized with
NoInit which was the initialization in version 2.2 of the Modelica
standard library).
</p>

<p>
Based on the setting of initType, the integrator (I) and derivative (D)
blocks inside the PID controller are initialized according to the following table:
</p>

<table border=1 cellspacing=0 cellpadding=2>
  <tr><td valign=\"top\"><b>initType</b></td>
      <td valign=\"top\"><b>I.initType</b></td>
      <td valign=\"top\"><b>D.initType</b></td></tr>

  <tr><td valign=\"top\"><b>NoInit</b></td>
      <td valign=\"top\">NoInit</td>
      <td valign=\"top\">NoInit</td></tr>

  <tr><td valign=\"top\"><b>SteadyState</b></td>
      <td valign=\"top\">SteadyState</td>
      <td valign=\"top\">SteadyState</td></tr>

  <tr><td valign=\"top\"><b>InitialState</b></td>
      <td valign=\"top\">InitialState</td>
      <td valign=\"top\">InitialState</td></tr>

  <tr><td valign=\"top\"><b>InitialOutput</b><br>
          and initial equation: y = y_start</td>
      <td valign=\"top\">NoInit</td>
      <td valign=\"top\">SteadyState</td></tr>

  <tr><td valign=\"top\"><b>DoNotUse_InitialIntegratorState</b></td>
      <td valign=\"top\">InitialState</td>
      <td valign=\"top\">NoInit</td></tr>
</table>

<p>
In many cases, the most useful initial condition is
<b>SteadyState</b> because initial transients are then no longer
present. If initType = InitPID.SteadyState, then in some
cases difficulties might occur. The reason is the
equation of the integrator:
</p>

<pre>
   <b>der</b>(y) = k*u;
</pre>

<p>
The steady state equation \"der(x)=0\" leads to the condition that the input u to the
integrator is zero. If the input u is already (directly or indirectly) defined
by another initial condition, then the initialization problem is <b>singular</b>
(has none or infinitely many solutions). This situation occurs often
for mechanical systems, where, e.g., u = desiredSpeed - measuredSpeed and
since speed is both a state and a derivative, it is natural to
initialize it with zero. As sketched this is, however, not possible.
The solution is to not initialize u or the variable that is used
to compute u by an algebraic equation.
</p>

</html>"));
  end PID;

  block LimPID
    "P, PI, PD, and PID controller with limited output, anti-windup compensation and setpoint weighting"
    import Modelica.Blocks.Types.InitPID;
    import Modelica.Blocks.Types.Init;
    import Modelica.Blocks.Types.SimpleController;
    extends Interfaces.SVcontrol;
    output Real controlError = u_s - u_m
      "Control error (set point - measurement)";

    parameter .Modelica.Blocks.Types.SimpleController controllerType=
           .Modelica.Blocks.Types.SimpleController.PID "Type of controller";
    parameter Real k(min=0, unit="1") = 1 "Gain of controller";
    parameter SIunits.Time Ti(min=Modelica.Constants.small)=0.5
      "Time constant of Integrator block"
       annotation(Dialog(enable=controllerType==.Modelica.Blocks.Types.SimpleController.PI or
                                controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter SIunits.Time Td(min=0)= 0.1 "Time constant of Derivative block"
         annotation(Dialog(enable=controllerType==.Modelica.Blocks.Types.SimpleController.PD or
                                  controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter Real yMax(start=1) "Upper limit of output";
    parameter Real yMin=-yMax "Lower limit of output";
    parameter Real wp(min=0) = 1
      "Set-point weight for Proportional block (0..1)";
    parameter Real wd(min=0) = 0 "Set-point weight for Derivative block (0..1)"
         annotation(Dialog(enable=controllerType==.Modelica.Blocks.Types.SimpleController.PD or
                                  controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter Real Ni(min=100*Modelica.Constants.eps) = 0.9
      "Ni*Ti is time constant of anti-windup compensation"
       annotation(Dialog(enable=controllerType==.Modelica.Blocks.Types.SimpleController.PI or
                                controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter Real Nd(min=100*Modelica.Constants.eps) = 10
      "The higher Nd, the more ideal the derivative block"
         annotation(Dialog(enable=controllerType==.Modelica.Blocks.Types.SimpleController.PD or
                                  controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter .Modelica.Blocks.Types.InitPID initType= .Modelica.Blocks.Types.InitPID.DoNotUse_InitialIntegratorState
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                       annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Boolean limitsAtInit = true
      "= false, if limits are ignored during initialization"
      annotation(Evaluate=true, Dialog(group="Initialization"));
    parameter Real xi_start=0
      "Initial or guess value value for integrator output (= integrator state)"
      annotation (Dialog(group="Initialization",
                  enable=controllerType==.Modelica.Blocks.Types.SimpleController.PI or
                         controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter Real xd_start=0
      "Initial or guess value for state of derivative block"
      annotation (Dialog(group="Initialization",
                           enable=controllerType==.Modelica.Blocks.Types.SimpleController.PD or
                                  controllerType==.Modelica.Blocks.Types.SimpleController.PID));
    parameter Real y_start=0 "Initial value of output"
      annotation(Dialog(enable=initType == .Modelica.Blocks.Types.InitPID.InitialOutput, group=
            "Initialization"));
    parameter Boolean strict=false "= true, if strict limits with noEvent(..)"
      annotation (Evaluate=true, choices(checkBox=true), Dialog(tab="Advanced"));
    constant SI.Time unitTime=1  annotation(HideResult=true);
    Blocks.Math.Add addP(k1=wp, k2=-1)
      annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
    Blocks.Math.Add addD(k1=wd, k2=-1) if with_D
      annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
    Blocks.Math.Gain P(k=1)
                       annotation (Placement(transformation(extent={{-40,40},{
              -20,60}})));
    Blocks.Continuous.Integrator I(k=unitTime/Ti, y_start=xi_start,
      initType=if initType==InitPID.SteadyState then
                  Init.SteadyState else
               if initType==InitPID.InitialState or
                  initType==InitPID.DoNotUse_InitialIntegratorState then
                  Init.InitialState else Init.NoInit) if with_I
      annotation (Placement(transformation(extent={{-40,-60},{-20,-40}})));
    Blocks.Continuous.Derivative D(k=Td/unitTime, T=max([Td/Nd, 1.e-14]), x_start=xd_start,
      initType=if initType==InitPID.SteadyState or
                  initType==InitPID.InitialOutput then Init.SteadyState else
               if initType==InitPID.InitialState then Init.InitialState else
                  Init.NoInit) if with_D
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    Blocks.Math.Gain gainPID(k=k) annotation (Placement(transformation(extent={
              {30,-10},{50,10}})));
    Blocks.Math.Add3 addPID annotation (Placement(transformation(
            extent={{0,-10},{20,10}})));
    Blocks.Math.Add3 addI(k2=-1) if with_I annotation (Placement(
          transformation(extent={{-80,-60},{-60,-40}})));
    Blocks.Math.Add addSat(k1=+1, k2=-1) if
                                     with_I
      annotation (Placement(transformation(
          origin={80,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Blocks.Math.Gain gainTrack(k=1/(k*Ni)) if with_I
      annotation (Placement(transformation(extent={{40,-80},{20,-60}})));
    Blocks.Nonlinear.Limiter limiter(uMax=yMax, uMin=yMin, strict=strict, limitsAtInit=limitsAtInit)
      annotation (Placement(transformation(extent={{70,-10},{90,10}})));
  protected
    parameter Boolean with_I = controllerType==SimpleController.PI or
                               controllerType==SimpleController.PID annotation(Evaluate=true, HideResult=true);
    parameter Boolean with_D = controllerType==SimpleController.PD or
                               controllerType==SimpleController.PID annotation(Evaluate=true, HideResult=true);
  public
    Sources.Constant Dzero(k=0) if not with_D
      annotation (Placement(transformation(extent={{-30,20},{-20,30}})));
    Sources.Constant Izero(k=0) if not with_I
      annotation (Placement(transformation(extent={{10,-55},{0,-45}})));
  initial equation
    if initType==InitPID.InitialOutput then
       gainPID.y = y_start;
    end if;
  equation
    if initType == InitPID.InitialOutput and (y_start < yMin or y_start > yMax) then
        Modelica.Utilities.Streams.error("LimPID: Start value y_start (=" + String(y_start) +
           ") is outside of the limits of yMin (=" + String(yMin) +") and yMax (=" + String(yMax) + ")");
    end if;

    connect(u_s, addP.u1) annotation (Line(points={{-120,0},{-96,0},{-96,56},{
            -82,56}}, color={0,0,127}));
    connect(u_s, addD.u1) annotation (Line(points={{-120,0},{-96,0},{-96,6},{
            -82,6}}, color={0,0,127}));
    connect(u_s, addI.u1) annotation (Line(points={{-120,0},{-96,0},{-96,-42},{
            -82,-42}}, color={0,0,127}));
    connect(addP.y, P.u) annotation (Line(points={{-59,50},{-42,50}}, color={0,
            0,127}));
    connect(addD.y, D.u)
      annotation (Line(points={{-59,0},{-42,0}}, color={0,0,127}));
    connect(addI.y, I.u) annotation (Line(points={{-59,-50},{-42,-50}}, color={
            0,0,127}));
    connect(P.y, addPID.u1) annotation (Line(points={{-19,50},{-10,50},{-10,8},
            {-2,8}}, color={0,0,127}));
    connect(D.y, addPID.u2)
      annotation (Line(points={{-19,0},{-2,0}}, color={0,0,127}));
    connect(I.y, addPID.u3) annotation (Line(points={{-19,-50},{-10,-50},{-10,
            -8},{-2,-8}}, color={0,0,127}));
    connect(addPID.y, gainPID.u)
      annotation (Line(points={{21,0},{28,0}}, color={0,0,127}));
    connect(gainPID.y, addSat.u2) annotation (Line(points={{51,0},{60,0},{60,
            -20},{74,-20},{74,-38}}, color={0,0,127}));
    connect(gainPID.y, limiter.u)
      annotation (Line(points={{51,0},{68,0}}, color={0,0,127}));
    connect(limiter.y, addSat.u1) annotation (Line(points={{91,0},{94,0},{94,
            -20},{86,-20},{86,-38}}, color={0,0,127}));
    connect(limiter.y, y)
      annotation (Line(points={{91,0},{110,0}}, color={0,0,127}));
    connect(addSat.y, gainTrack.u) annotation (Line(points={{80,-61},{80,-70},{
            42,-70}}, color={0,0,127}));
    connect(gainTrack.y, addI.u3) annotation (Line(points={{19,-70},{-88,-70},{
            -88,-58},{-82,-58}}, color={0,0,127}));
    connect(u_m, addP.u2) annotation (Line(
        points={{0,-120},{0,-92},{-92,-92},{-92,44},{-82,44}},
        color={0,0,127},
        thickness=0.5));
    connect(u_m, addD.u2) annotation (Line(
        points={{0,-120},{0,-92},{-92,-92},{-92,-6},{-82,-6}},
        color={0,0,127},
        thickness=0.5));
    connect(u_m, addI.u2) annotation (Line(
        points={{0,-120},{0,-92},{-92,-92},{-92,-50},{-82,-50}},
        color={0,0,127},
        thickness=0.5));
    connect(Dzero.y, addPID.u2) annotation (Line(points={{-19.5,25},{-14,25},{
            -14,0},{-2,0}}, color={0,0,127}));
    connect(Izero.y, addPID.u3) annotation (Line(points={{-0.5,-50},{-10,-50},{
            -10,-8},{-2,-8}}, color={0,0,127}));
    annotation (defaultComponentName="PID",
      Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Line(points={{-80,78},{-80,-90}}, color={192,192,192}),
          Polygon(
            points={{-80,90},{-88,68},{-72,68},{-80,90}},
            lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,-80},{82,-80}}, color={192,192,192}),
          Polygon(
            points={{90,-80},{68,-72},{68,-88},{90,-80}},
            lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid),
          Line(points={{-80,-80},{-80,-20},{30,60},{80,60}}, color={0,0,127}),
          Text(
            extent={{-20,-20},{80,-60}},
            lineColor={192,192,192},
            textString="%controllerType"),
          Line(
            visible=strict,
            points={{30,60},{81,60}},
            color={255,0,0})}),
      Documentation(info="<html>
<p>
Via parameter <b>controllerType</b> either <b>P</b>, <b>PI</b>, <b>PD</b>,
or <b>PID</b> can be selected. If, e.g., PI is selected, all components belonging to the
D-part are removed from the block (via conditional declarations).
The example model
<a href=\"modelica://Modelica.Blocks.Examples.PID_Controller\">Modelica.Blocks.Examples.PID_Controller</a>
demonstrates the usage of this controller.
Several practical aspects of PID controller design are incorporated
according to chapter 3 of the book:
</p>

<dl>
<dt>&Aring;str&ouml;m K.J., and H&auml;gglund T.:</dt>
<dd> <b>PID Controllers: Theory, Design, and Tuning</b>.
     Instrument Society of America, 2nd edition, 1995.
</dd>
</dl>

<p>
Besides the additive <b>proportional, integral</b> and <b>derivative</b>
part of this controller, the following features are present:
</p>
<ul>
<li> The output of this controller is limited. If the controller is
     in its limits, anti-windup compensation is activated to drive
     the integrator state to zero. </li>
<li> The high-frequency gain of the derivative part is limited
     to avoid excessive amplification of measurement noise.</li>
<li> Setpoint weighting is present, which allows to weight
     the setpoint in the proportional and the derivative part
     independently from the measurement. The controller will respond
     to load disturbances and measurement noise independently of this setting
     (parameters wp, wd). However, setpoint changes will depend on this
     setting. For example, it is useful to set the setpoint weight wd
     for the derivative part to zero, if steps may occur in the
     setpoint signal.</li>
</ul>

<p>
The parameters of the controller can be manually adjusted by performing
simulations of the closed loop system (= controller + plant connected
together) and using the following strategy:
</p>

<ol>
<li> Set very large limits, e.g., yMax = Modelica.Constants.inf</li>
<li> Select a <b>P</b>-controller and manually enlarge parameter <b>k</b>
     (the total gain of the controller) until the closed-loop response
     cannot be improved any more.</li>
<li> Select a <b>PI</b>-controller and manually adjust parameters
     <b>k</b> and <b>Ti</b> (the time constant of the integrator).
     The first value of Ti can be selected, such that it is in the
     order of the time constant of the oscillations occurring with
     the P-controller. If, e.g., vibrations in the order of T=10 ms
     occur in the previous step, start with Ti=0.01 s.</li>
<li> If you want to make the reaction of the control loop faster
     (but probably less robust against disturbances and measurement noise)
     select a <b>PID</b>-Controller and manually adjust parameters
     <b>k</b>, <b>Ti</b>, <b>Td</b> (time constant of derivative block).</li>
<li> Set the limits yMax and yMin according to your specification.</li>
<li> Perform simulations such that the output of the PID controller
     goes in its limits. Tune <b>Ni</b> (Ni*Ti is the time constant of
     the anti-windup compensation) such that the input to the limiter
     block (= limiter.u) goes quickly enough back to its limits.
     If Ni is decreased, this happens faster. If Ni=infinity, the
     anti-windup compensation is switched off and the controller works bad.</li>
</ol>

<p>
<b>Initialization</b>
</p>

<p>
This block can be initialized in different
ways controlled by parameter <b>initType</b>. The possible
values of initType are defined in
<a href=\"modelica://Modelica.Blocks.Types.InitPID\">Modelica.Blocks.Types.InitPID</a>.
This type is identical to
<a href=\"modelica://Modelica.Blocks.Types.Init\">Types.Init</a>,
with the only exception that the additional option
<b>DoNotUse_InitialIntegratorState</b> is added for
backward compatibility reasons (= integrator is initialized with
InitialState whereas differential part is initialized with
NoInit which was the initialization in version 2.2 of the Modelica
standard library).
</p>

<p>
Based on the setting of initType, the integrator (I) and derivative (D)
blocks inside the PID controller are initialized according to the following table:
</p>

<table border=1 cellspacing=0 cellpadding=2>
  <tr><td valign=\"top\"><b>initType</b></td>
      <td valign=\"top\"><b>I.initType</b></td>
      <td valign=\"top\"><b>D.initType</b></td></tr>

  <tr><td valign=\"top\"><b>NoInit</b></td>
      <td valign=\"top\">NoInit</td>
      <td valign=\"top\">NoInit</td></tr>

  <tr><td valign=\"top\"><b>SteadyState</b></td>
      <td valign=\"top\">SteadyState</td>
      <td valign=\"top\">SteadyState</td></tr>

  <tr><td valign=\"top\"><b>InitialState</b></td>
      <td valign=\"top\">InitialState</td>
      <td valign=\"top\">InitialState</td></tr>

  <tr><td valign=\"top\"><b>InitialOutput</b><br>
          and initial equation: y = y_start</td>
      <td valign=\"top\">NoInit</td>
      <td valign=\"top\">SteadyState</td></tr>

  <tr><td valign=\"top\"><b>DoNotUse_InitialIntegratorState</b></td>
      <td valign=\"top\">InitialState</td>
      <td valign=\"top\">NoInit</td></tr>
</table>

<p>
In many cases, the most useful initial condition is
<b>SteadyState</b> because initial transients are then no longer
present. If initType = InitPID.SteadyState, then in some
cases difficulties might occur. The reason is the
equation of the integrator:
</p>

<pre>
   <b>der</b>(y) = k*u;
</pre>

<p>
The steady state equation \"der(x)=0\" leads to the condition that the input u to the
integrator is zero. If the input u is already (directly or indirectly) defined
by another initial condition, then the initialization problem is <b>singular</b>
(has none or infinitely many solutions). This situation occurs often
for mechanical systems, where, e.g., u = desiredSpeed - measuredSpeed and
since speed is both a state and a derivative, it is natural to
initialize it with zero. As sketched this is, however, not possible.
The solution is to not initialize u_m or the variable that is used
to compute u_m by an algebraic equation.
</p>

<p>
If parameter <b>limitAtInit</b> = <b>false</b>, the limits at the
output of this controller block are removed from the initialization problem which
leads to a much simpler equation system. After initialization has been
performed, it is checked via an assert whether the output is in the
defined limits. For backward compatibility reasons
<b>limitAtInit</b> = <b>true</b>. In most cases it is best
to use <b>limitAtInit</b> = <b>false</b>.
</p>
</html>"));
  end LimPID;

  block TransferFunction "Linear transfer function"
    import Modelica.Blocks.Types.Init;
    extends Interfaces.SISO;

    parameter Real b[:]={1}
      "Numerator coefficients of transfer function (e.g., 2*s+3 is specified as {2,3})";
    parameter Real a[:]={1}
      "Denominator coefficients of transfer function (e.g., 5*s+6 is specified as {5,6})";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                       annotation(Evaluate=true, Dialog(group=
            "Initialization"));
    parameter Real x_start[size(a, 1) - 1]=zeros(nx)
      "Initial or guess values of states"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start=0
      "Initial value of output (derivatives of y are zero up to nx-1-th derivative)"
      annotation(Dialog(enable=initType == Init.InitialOutput, group=
            "Initialization"));
    output Real x[size(a, 1) - 1](start=x_start)
      "State of transfer function from controller canonical form";
  protected
    parameter Integer na=size(a, 1) "Size of Denominator of transfer function.";
    parameter Integer nb=size(b, 1) "Size of Numerator of transfer function.";
    parameter Integer nx=size(a, 1) - 1;
    parameter Real bb[:] = vector([zeros(max(0,na-nb),1);b]);
    parameter Real d = bb[1]/a[1];
    parameter Real a_end = if a[end] > 100*Modelica.Constants.eps*sqrt(a*a) then a[end] else 1.0;
    Real x_scaled[size(x,1)] "Scaled vector x";

  initial equation
    if initType == Init.SteadyState then
      der(x_scaled) = zeros(nx);
    elseif initType == Init.InitialState then
      x_scaled = x_start*a_end;
    elseif initType == Init.InitialOutput then
      y = y_start;
      der(x_scaled[2:nx]) = zeros(nx-1);
    end if;
  equation
    assert(size(b,1) <= size(a,1), "Transfer function is not proper");
    if nx == 0 then
       y = d*u;
    else
       der(x_scaled[1])    = (-a[2:na]*x_scaled + a_end*u)/a[1];
       der(x_scaled[2:nx]) = x_scaled[1:nx-1];
       y = ((bb[2:na] - d*a[2:na])*x_scaled)/a_end + d*u;
       x = x_scaled/a_end;
    end if;
    annotation (
      Documentation(info="<html>
<p>
This block defines the transfer function between the input
u and the output y
as (nb = dimension of b, na = dimension of a):
</p>
<pre>
           b[1]*s^[nb-1] + b[2]*s^[nb-2] + ... + b[nb]
   y(s) = --------------------------------------------- * u(s)
           a[1]*s^[na-1] + a[2]*s^[na-2] + ... + a[na]
</pre>
<p>
State variables <b>x</b> are defined according to <b>controller canonical</b>
form. Internally, vector <b>x</b> is scaled to improve the numerics (the states in versions before version 3.0 of the Modelica Standard Library have been not scaled). This scaling is
not visible from the outside of this block because the non-scaled vector <b>x</b>
is provided as output signal and the start value is with respect to the non-scaled
vector <b>x</b>.
Initial values of the states <b>x</b> can be set via parameter <b>x_start</b>.
</p>

<p>
Example:
</p>
<pre>
     TransferFunction g(b = {2,4}, a = {1,3});
</pre>
<p>
results in the following transfer function:
</p>
<pre>
        2*s + 4
   y = --------- * u
         s + 3
</pre>
</html>"),
      Icon(
          coordinateSystem(preserveAspectRatio=true,
            extent={{-100.0,-100.0},{100.0,100.0}}),
            graphics={
          Line(points={{-80.0,0.0},{80.0,0.0}},
            color={0,0,127}),
        Text(lineColor={0,0,127},
          extent={{-90.0,10.0},{90.0,90.0}},
          textString="b(s)"),
        Text(lineColor={0,0,127},
          extent={{-90.0,-90.0},{90.0,-10.0}},
          textString="a(s)")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Line(points={{40,0},{-40,0}}),
          Text(
            extent={{-55,55},{55,5}},
            lineColor={0,0,0},
            textString="b(s)"),
          Text(
            extent={{-55,-5},{55,-55}},
            lineColor={0,0,0},
            textString="a(s)"),
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255})}));
  end TransferFunction;

  block StateSpace "Linear state space system"
    import Modelica.Blocks.Types.Init;
    parameter Real A[:, size(A, 1)]=[1, 0; 0, 1]
      "Matrix A of state space model (e.g., A=[1, 0; 0, 1])";
    parameter Real B[size(A, 1), :]=[1; 1]
      "Matrix B of state space model (e.g., B=[1; 1])";
    parameter Real C[:, size(A, 1)]=[1, 1]
      "Matrix C of state space model (e.g., C=[1, 1])";
    parameter Real D[size(C, 1), size(B, 2)]=zeros(size(C, 1), size(B, 2))
      "Matrix D of state space model";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real x_start[nx]=zeros(nx) "Initial or guess values of states"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start[ny]=zeros(ny)
      "Initial values of outputs (remaining states are in steady state if possible)"
      annotation(Dialog(enable=initType == Init.InitialOutput, group=
            "Initialization"));

    extends Interfaces.MIMO(final nin=size(B, 2), final nout=size(C, 1));
    output Real x[size(A, 1)](start=x_start) "State vector";

  protected
    parameter Integer nx = size(A, 1) "number of states";
    parameter Integer ny = size(C, 1) "number of outputs";
  initial equation
    if initType == Init.SteadyState then
      der(x) = zeros(nx);
    elseif initType == Init.InitialState then
      x = x_start;
    elseif initType == Init.InitialOutput then
      x = Modelica.Math.Matrices.equalityLeastSquares(A, -B*u, C, y_start - D*u);
    end if;
  equation
    der(x) = A*x + B*u;
    y = C*x + D*u;
    annotation (
      Documentation(info="<html>
<p>
The State Space block defines the relation
between the input u and the output
y in state space form:
</p>
<pre>

    der(x) = A * x + B * u
        y  = C * x + D * u
</pre>
<p>
The input is a vector of length nu, the output is a vector
of length ny and nx is the number of states. Accordingly
</p>
<pre>
        A has the dimension: A(nx,nx),
        B has the dimension: B(nx,nu),
        C has the dimension: C(ny,nx),
        D has the dimension: D(ny,nu)
</pre>
<p>
Example:
</p>
<pre>
     parameter: A = [0.12, 2;3, 1.5]
     parameter: B = [2, 7;3, 1]
     parameter: C = [0.1, 2]
     parameter: D = zeros(ny,nu)
results in the following equations:
  [der(x[1])]   [0.12  2.00] [x[1]]   [2.0  7.0] [u[1]]
  [         ] = [          ]*[    ] + [        ]*[    ]
  [der(x[2])]   [3.00  1.50] [x[2]]   [0.1  2.0] [u[2]]
                             [x[1]]            [u[1]]
       y[1]   = [0.1  2.0] * [    ] + [0  0] * [    ]
                             [x[2]]            [u[2]]
</pre>
</html>"),   Icon(
      coordinateSystem(preserveAspectRatio=true,
        extent={{-100,-100},{100,100}}),
        graphics={
      Text(extent={{-90,10},{-10,90}},
        textString="A",
        lineColor={0,0,127}),
      Text(extent={{10,10},{90,90}},
        textString="B",
        lineColor={0,0,127}),
      Text(extent={{-90,-10},{-10,-90}},
        textString="C",
        lineColor={0,0,127}),
      Text(extent={{10,-10},{90,-90}},
        textString="D",
        lineColor={0,0,127}),
      Line(points={{0,-90},{0,90}},
        color={192,192,192}),
      Line(points={{-90,0},{90,0}},
        color={192,192,192})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}}), graphics={
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Text(
            extent={{-60,40},{60,0}},
            lineColor={0,0,0},
            textString="sx=Ax+Bu"),
          Text(
            extent={{-60,0},{60,-40}},
            lineColor={0,0,0},
            textString=" y=Cx+Du"),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255})}));
  end StateSpace;

  block Der "Derivative of input (= analytic differentiations)"
      extends Interfaces.SISO;

  equation
    y = der(u);
      annotation (defaultComponentName="der1",
   Icon(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},{100,100}}),
          graphics={Text(
            extent={{-96,28},{94,-24}},
            textString="der()",
            lineColor={0,0,127})}),
          Documentation(info="<html>
<p>
Defines that the output y is the <i>derivative</i>
of the input u. Note, that Modelica.Blocks.Continuous.Derivative
computes the derivative in an approximate sense, where as this block computes
the derivative exactly. This requires that the input u is differentiated
by the Modelica translator, if this derivative is not yet present in
the model.
</p>
</html>"));
  end Der;

  block LowpassButterworth
    "Output the input signal filtered with a low pass Butterworth filter of any order"

    import Modelica.Blocks.Types.Init;
    import Modelica.Constants.pi;

    extends Modelica.Blocks.Interfaces.SISO;

    parameter Integer n(min=1) = 2 "Order of filter";
    parameter SI.Frequency f(start=1) "Cut-off frequency";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real x1_start[m]=zeros(m)
      "Initial or guess values of states 1 (der(x1)=x2)"
      annotation (Dialog(group="Initialization"));
    parameter Real x2_start[m]=zeros(m) "Initial or guess values of states 2"
      annotation (Dialog(group="Initialization"));
    parameter Real xr_start=0.0
      "Initial or guess value of real pole for uneven order otherwise dummy"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start=0.0
      "Initial value of output (states are initialized in steady state if possible)"
       annotation(Dialog(enable=initType == Init.InitialOutput, group=
            "Initialization"));

    output Real x1[m](start=x1_start)
      "states 1 of second order filters (der(x1) = x2)";
    output Real x2[m](start=x2_start) "states 2 of second order filters";
    output Real xr(start=xr_start)
      "state of real pole for uneven order otherwise dummy";
  protected
    parameter Integer m=integer(n/2);
    parameter Boolean evenOrder = 2*m == n;
    parameter Real w=2*pi*f;
    Real z[m + 1];
    Real polereal[m];
    Real poleimag[m];
    Real realpol;
    Real k2[m];
    Real D[m];
    Real w0[m];
    Real k1;
    Real T;
  initial equation
    if initType == Init.SteadyState then
      der(x1) = zeros(m);
      der(x2) = zeros(m);
      if not evenOrder then
        der(xr) = 0.0;
      end if;
    elseif initType == Init.InitialState then
      x1 = x1_start;
      x2 = x2_start;
      if not evenOrder then
        xr = xr_start;
      end if;
    elseif initType == Init.InitialOutput then
      y = y_start;
      der(x1) = zeros(m);
      if evenOrder then
        if m > 1 then
          der(x2[1:m-1]) = zeros(m-1);
        end if;
      else
        der(x1) = zeros(m);
      end if;
    end if;
  equation
    k2 = ones(m);
    k1 = 1;
    z[1] = u;

    // calculate filter parameters
    for i in 1:m loop
      // poles of prototype lowpass
      polereal[i] = cos(pi/2 + pi/n*(i - 0.5));
      poleimag[i] = sin(pi/2 + pi/n*(i - 0.5));
      // scaling and calculation of second order filter coefficients
      w0[i] = (polereal[i]^2 + poleimag[i]^2)*w;
      D[i] = -polereal[i]/w0[i]*w;
    end for;
    realpol = 1*w;
    T = 1/realpol;

    // calculate second order filters
    for i in 1:m loop
      der(x1[i]) = x2[i];
      der(x2[i]) = k2[i]*w0[i]^2*z[i] - 2*D[i]*w0[i]*x2[i] - w0[i]^2*x1[i];
      z[i + 1] = x1[i];
    end for;

    // calculate first order filter if necessary
    if evenOrder then
      // even order
      xr = 0;
      y = z[m + 1];
    else
      // uneven order
      der(xr) = (k1*z[m + 1] - xr)/T;
      y = xr;
    end if;
    annotation (
      Icon(
          coordinateSystem(preserveAspectRatio=true,
              extent={{-100.0,-100.0},{100.0,100.0}}),
              graphics={
          Line(points={{-80.0,78.0},{-80.0,-90.0}},
              color={192,192,192}),
          Polygon(lineColor={192,192,192},
              fillColor={192,192,192},
              fillPattern=FillPattern.Solid,
              points={{-79.5584,91.817},{-87.5584,69.817},{-71.5584,69.817},{-79.5584,91.817}}),
          Line(origin = {-1.939,-1.816},
              points = {{81.939,36.056},{65.362,36.056},{14.39,-26.199},{-29.966,113.485},{-65.374,-61.217},{-78.061,-78.184}},
              color = {0,0,127},
              smooth = Smooth.Bezier),
          Line(points={{-90.9779,-80.7697},{81.0221,-80.7697}},
              color={192,192,192}),
          Polygon(lineColor={192,192,192},
              fillColor={192,192,192},
              fillPattern=FillPattern.Solid,
              points={{91.3375,-79.8233},{69.3375,-71.8233},{69.3375,-87.8233},{91.3375,-79.8233}}),
          Text(lineColor={192,192,192},
              extent={{-45.1735,-68.0},{92.0,-11.47}},
              textString="LowpassButterworthFilter"),
          Text(extent={{8.0,-146.0},{8.0,-106.0}},
              textString="f=%f"),
          Text(lineColor={192,192,192},
              extent={{-2.0,48.0},{94.0,94.0}},
              textString="%n")}),
      Diagram(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},{
              100,100}}), graphics={
          Line(points={{40,0},{-40,0}}),
          Text(
            extent={{-55,55},{55,5}},
            lineColor={0,0,0},
            textString="1"),
          Text(
            extent={{-55,-5},{55,-55}},
            lineColor={0,0,0},
            textString="a(s)"),
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255})}),
      Documentation(info="<html>
<p>
This block defines the transfer function between the input u
and the output y as an n-th order low pass filter with <i>Butterworth</i>
characteristics and cut-off frequency f. It is implemented as
a series of second order filters and a first order filter.
Butterworth filters have the feature that the amplitude at the
cut-off frequency f is 1/sqrt(2) (= 3 dB), i.e., they are
always \"normalized\". Step responses of the Butterworth filter of
different orders are shown in the next figure:
</p>

<p>
<img src=\"modelica://Modelica/Resources/Images/Blocks/Butterworth.png\"
     alt=\"Butterworth.png\">
</p>

<p>
If transients at the simulation start shall be avoided, the filter
should be initialized in steady state (e.g., using option
initType=Modelica.Blocks.Types.Init.SteadyState).
</p>

</html>"));
  end LowpassButterworth;

  block CriticalDamping
    "Output the input signal filtered with an n-th order filter with critical damping"

    import Modelica.Blocks.Types.Init;
    extends Modelica.Blocks.Interfaces.SISO;

    parameter Integer n=2 "Order of filter";
    parameter Modelica.SIunits.Frequency f(start=1) "Cut-off frequency";
    parameter Boolean normalized = true
      "= true, if amplitude at f_cut is 3 dB, otherwise unmodified filter";
    parameter Modelica.Blocks.Types.Init initType=Modelica.Blocks.Types.Init.NoInit
      "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)"
                                                                                      annotation(Evaluate=true,
        Dialog(group="Initialization"));
    parameter Real x_start[n]=zeros(n) "Initial or guess values of states"
      annotation (Dialog(group="Initialization"));
    parameter Real y_start=0.0
      "Initial value of output (remaining states are in steady state)"
      annotation(Dialog(enable=initType == Init.InitialOutput, group=
            "Initialization"));

    output Real x[n](start=x_start) "Filter states";
  protected
    parameter Real alpha=if normalized then sqrt(2^(1/n) - 1) else 1.0
      "Frequency correction factor for normalized filter";
    parameter Real w=2*Modelica.Constants.pi*f/alpha;
  initial equation
    if initType == Init.SteadyState then
      der(x) = zeros(n);
    elseif initType == Init.InitialState then
      x = x_start;
    elseif initType == Init.InitialOutput then
      y = y_start;
      der(x[1:n-1]) = zeros(n-1);
    end if;
  equation
    der(x[1]) = (u - x[1])*w;
    for i in 2:n loop
      der(x[i]) = (x[i - 1] - x[i])*w;
    end for;
    y = x[n];
    annotation (
      Icon(
          coordinateSystem(preserveAspectRatio=true,
            extent={{-100.0,-100.0},{100.0,100.0}}),
            graphics={
          Line(points={{-80.6897,77.6256},{-80.6897,-90.3744}},
            color={192,192,192}),
          Polygon(lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid,
            points={{-79.7044,90.6305},{-87.7044,68.6305},{-71.7044,68.6305},{-79.7044,90.6305}}),
          Line(points={{-90.0,-80.0},{82.0,-80.0}},
            color={192,192,192}),
          Polygon(lineColor={192,192,192},
            fillColor={192,192,192},
            fillPattern=FillPattern.Solid,
            points={{90.0,-80.0},{68.0,-72.0},{68.0,-88.0},{90.0,-80.0}}),
          Text(lineColor={192,192,192},
            extent={{0.0,-60.0},{60.0,0.0}},
            textString="PTn"),
          Line(origin = {-17.976,-6.521},
            points = {{96.962,55.158},{16.42,50.489},{-18.988,18.583},{-32.024,-53.479},{-62.024,-73.479}},
            color = {0,0,127},
            smooth = Smooth.Bezier),
          Text(lineColor={192,192,192},
            extent={{-70.0,48.0},{26.0,94.0}},
            textString="%n"),
          Text(extent={{8.0,-146.0},{8.0,-106.0}},
            textString="f=%f")}),
      Diagram(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},{
              100,100}}), graphics={
          Line(points={{40,0},{-40,0}}),
          Text(
            extent={{-55,55},{55,5}},
            lineColor={0,0,0},
            textString="1"),
          Rectangle(extent={{-60,60},{60,-60}}, lineColor={0,0,255}),
          Line(points={{-100,0},{-60,0}}, color={0,0,255}),
          Line(points={{60,0},{100,0}}, color={0,0,255}),
          Text(
            extent={{-54,-6},{44,-56}},
            lineColor={0,0,0},
            textString="(s/w + 1)"),
          Text(
            extent={{38,-10},{58,-30}},
            lineColor={0,0,0},
            textString="n")}),
      Documentation(info="<html>
<p>This block defines the transfer function between the
input u and the output y
as an n-th order filter with <i>critical damping</i>
characteristics and cut-off frequency f. It is
implemented as a series of first order filters.
This filter type is especially useful to filter the input of an
inverse model, since the filter does not introduce any transients.
</p>

<p>
If parameter <b>normalized</b> = <b>true</b> (default), the filter
is normalized such that the amplitude of the filter transfer function
at the cut-off frequency f is 1/sqrt(2) (= 3 dB). Otherwise, the filter
is not normalized, i.e., it is unmodified. A normalized filter is usually
much better for applications, since filters of different orders are
\"comparable\", whereas non-normalized filters usually require to adapt the
cut-off frequency, when the order of the filter is changed.
Figures of the filter step responses are shown below.
Note, in versions before version 3.0 of the Modelica Standard library,
the CriticalDamping filter was provided only in non-normalized form.
</p>

<p>If transients at the simulation start shall be avoided, the filter
should be initialized in steady state (e.g., using option
initType=Modelica.Blocks.Types.Init.SteadyState).
</p>

<p>
The critical damping filter is defined as
</p>

<pre>
    &alpha; = <b>if</b> normalized <b>then</b> <b>sqrt</b>(2^(1/n) - 1) <b>else</b> 1 // frequency correction factor
    &omega; = 2*&pi;*f/&alpha;
              1
    y = ------------- * u
         (s/w + 1)^n

</pre>

<p>
<img src=\"modelica://Modelica/Resources/Images/Blocks/CriticalDampingNormalized.png\"
     alt=\"CriticalDampingNormalized.png\">
</p>

<p>
<img src=\"modelica://Modelica/Resources/Images/Blocks/CriticalDampingNonNormalized.png\"
     alt=\"CriticalDampingNonNormalized.png\">
</p>

</html>"));
  end CriticalDamping;

  block Filter
    "Continuous low pass, high pass, band pass or band stop IIR-filter of type CriticalDamping, Bessel, Butterworth or ChebyshevI"
    import Modelica.Blocks.Continuous.Internal;

    extends Modelica.Blocks.Interfaces.SISO;

    parameter Modelica.Blocks.Types.AnalogFilter analogFilter=Modelica.Blocks.Types.AnalogFilter.CriticalDamping
      "Analog filter characteristics (CriticalDamping/Bessel/Butterworth/ChebyshevI)";
    parameter Modelica.Blocks.Types.FilterType filterType=Modelica.Blocks.Types.FilterType.LowPass
      "Type of filter (LowPass/HighPass/BandPass/BandStop)";
    parameter Integer order(min=1) = 2 "Order of filter";
    parameter Modelica.SIunits.Frequency f_cut "Cut-off frequency";
    parameter Real gain=1.0
      "Gain (= amplitude of frequency response at zero frequency)";
    parameter Real A_ripple(unit="dB") = 0.5
      "Pass band ripple for Chebyshev filter (otherwise not used); > 0 required"
      annotation(Dialog(enable=analogFilter==Modelica.Blocks.Types.AnalogFilter.ChebyshevI));
    parameter Modelica.SIunits.Frequency f_min=0
      "Band of band pass/stop filter is f_min (A=-3db*gain) .. f_cut (A=-3db*gain)"
      annotation(Dialog(enable=filterType == Modelica.Blocks.Types.FilterType.BandPass or
                               filterType == Modelica.Blocks.Types.FilterType.BandStop));
    parameter Boolean normalized=true
      "= true, if amplitude at f_cut = -3db, otherwise unmodified filter";
    parameter Modelica.Blocks.Types.Init init=Modelica.Blocks.Types.Init.SteadyState
      "Type of initialization (no init/steady state/initial state/initial output)"
      annotation(Evaluate=true, Dialog(tab="Advanced"));
    final parameter Integer nx = if filterType == Modelica.Blocks.Types.FilterType.LowPass or
                                    filterType == Modelica.Blocks.Types.FilterType.HighPass then
                                    order else 2*order;
    parameter Real x_start[nx] = zeros(nx) "Initial or guess values of states"
      annotation(Dialog(tab="Advanced"));
    parameter Real y_start = 0 "Initial value of output"
      annotation(Dialog(tab="Advanced"));
    parameter Real u_nominal = 1.0
      "Nominal value of input (used for scaling the states)"
    annotation(Dialog(tab="Advanced"));
    Modelica.Blocks.Interfaces.RealOutput x[nx] "Filter states";

  protected
    parameter Integer ncr = if analogFilter == Modelica.Blocks.Types.AnalogFilter.CriticalDamping then
                               order else mod(order,2);
    parameter Integer nc0 = if analogFilter == Modelica.Blocks.Types.AnalogFilter.CriticalDamping then
                               0 else integer(order/2);
    parameter Integer na = if filterType == Modelica.Blocks.Types.FilterType.BandPass or
                              filterType == Modelica.Blocks.Types.FilterType.BandStop then order else
                           if analogFilter == Modelica.Blocks.Types.AnalogFilter.CriticalDamping then
                              0 else integer(order/2);
    parameter Integer nr = if filterType == Modelica.Blocks.Types.FilterType.BandPass or
                              filterType == Modelica.Blocks.Types.FilterType.BandStop then 0 else
                           if analogFilter == Modelica.Blocks.Types.AnalogFilter.CriticalDamping then
                              order else mod(order,2);

    // Coefficients of prototype base filter (low pass filter with w_cut = 1 rad/s)
    parameter Real cr[ncr](each fixed=false);
    parameter Real c0[nc0](each fixed=false);
    parameter Real c1[nc0](each fixed=false);

    // Coefficients for differential equations.
    parameter Real r[nr](each fixed=false);
    parameter Real a[na](each fixed=false);
    parameter Real b[na](each fixed=false);
    parameter Real ku[na](each fixed=false);
    parameter Real k1[if filterType == Modelica.Blocks.Types.FilterType.LowPass then 0 else na](
                   each fixed = false);
    parameter Real k2[if filterType == Modelica.Blocks.Types.FilterType.LowPass then 0 else na](
                   each fixed = false);

    // Auxiliary variables
    Real uu[na+nr+1];

  initial equation
     if analogFilter == Modelica.Blocks.Types.AnalogFilter.CriticalDamping then
        cr = Internal.Filter.base.CriticalDamping(order, normalized);
     elseif analogFilter == Modelica.Blocks.Types.AnalogFilter.Bessel then
        (cr,c0,c1) = Internal.Filter.base.Bessel(order, normalized);
     elseif analogFilter == Modelica.Blocks.Types.AnalogFilter.Butterworth then
        (cr,c0,c1) = Internal.Filter.base.Butterworth(order, normalized);
     elseif analogFilter == Modelica.Blocks.Types.AnalogFilter.ChebyshevI then
        (cr,c0,c1) = Internal.Filter.base.ChebyshevI(order, A_ripple, normalized);
     end if;

     if filterType == Modelica.Blocks.Types.FilterType.LowPass then
        (r,a,b,ku) = Internal.Filter.roots.lowPass(cr,c0,c1,f_cut);
     elseif filterType == Modelica.Blocks.Types.FilterType.HighPass then
        (r,a,b,ku,k1,k2) = Internal.Filter.roots.highPass(cr,c0,c1,f_cut);
     elseif filterType == Modelica.Blocks.Types.FilterType.BandPass then
        (a,b,ku,k1,k2) = Internal.Filter.roots.bandPass(cr,c0,c1,f_min,f_cut);
     elseif filterType == Modelica.Blocks.Types.FilterType.BandStop then
        (a,b,ku,k1,k2) = Internal.Filter.roots.bandStop(cr,c0,c1,f_min,f_cut);
     end if;

     if init == Modelica.Blocks.Types.Init.InitialState then
        x = x_start;
     elseif init == Modelica.Blocks.Types.Init.SteadyState then
        der(x) = zeros(nx);
     elseif init == Modelica.Blocks.Types.Init.InitialOutput then
        y = y_start;
        if nx > 1 then
           der(x[1:nx-1]) = zeros(nx-1);
        end if;
     end if;

  equation
     assert(u_nominal > 0, "u_nominal > 0 required");
     assert(filterType == Modelica.Blocks.Types.FilterType.LowPass or
            filterType == Modelica.Blocks.Types.FilterType.HighPass or
            f_min > 0, "f_min > 0 required for band pass and band stop filter");
     assert(A_ripple > 0, "A_ripple > 0 required");
     assert(f_cut > 0, "f_cut > 0 required");

     /* All filters have the same basic differential equations:
        Real poles:
           der(x) = r*x - r*u
        Complex conjugate poles:
           der(x1) = a*x1 - b*x2 + ku*u;
           der(x2) = b*x1 + a*x2;
   */
     uu[1] = u/u_nominal;
     for i in 1:nr loop
        der(x[i]) = r[i]*(x[i] - uu[i]);
     end for;
     for i in 1:na loop
        der(x[nr+2*i-1]) = a[i]*x[nr+2*i-1] - b[i]*x[nr+2*i] + ku[i]*uu[nr+i];
        der(x[nr+2*i])   = b[i]*x[nr+2*i-1] + a[i]*x[nr+2*i];
     end for;

     // The output equation is different for the different filter types
     if filterType == Modelica.Blocks.Types.FilterType.LowPass then
        /* Low pass filter
           Real poles             :  y = x
           Complex conjugate poles:  y = x2
      */
        for i in 1:nr loop
           uu[i+1] = x[i];
        end for;
        for i in 1:na loop
           uu[nr+i+1] = x[nr+2*i];
        end for;

     elseif filterType == Modelica.Blocks.Types.FilterType.HighPass then
        /* High pass filter
           Real poles             :  y = -x + u;
           Complex conjugate poles:  y = k1*x1 + k2*x2 + u;
      */
        for i in 1:nr loop
           uu[i+1] = -x[i] + uu[i];
        end for;
        for i in 1:na loop
           uu[nr+i+1] = k1[i]*x[nr+2*i-1] + k2[i]*x[nr+2*i] + uu[nr+i];
        end for;

     elseif filterType == Modelica.Blocks.Types.FilterType.BandPass then
        /* Band pass filter
           Complex conjugate poles:  y = k1*x1 + k2*x2;
      */
        for i in 1:na loop
           uu[nr+i+1] = k1[i]*x[nr+2*i-1] + k2[i]*x[nr+2*i];
        end for;

     elseif filterType == Modelica.Blocks.Types.FilterType.BandStop then
        /* Band pass filter
           Complex conjugate poles:  y = k1*x1 + k2*x2 + u;
      */
        for i in 1:na loop
           uu[nr+i+1] = k1[i]*x[nr+2*i-1] + k2[i]*x[nr+2*i] + uu[nr+i];
        end for;

     else
        assert(false, "filterType (= " + String(filterType) + ") is unknown");
        uu = zeros(na+nr+1);
     end if;

     y = (gain*u_nominal)*uu[nr+na+1];

    annotation (
      Icon(
        coordinateSystem(preserveAspectRatio=true,
          extent={{-100.0,-100.0},{100.0,100.0}}),
          graphics={
        Line(points={{-80.0,80.0},{-80.0,-88.0}},
          color={192,192,192}),
        Polygon(lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid,
          points={{-80.0,92.0},{-88.0,70.0},{-72.0,70.0},{-80.0,92.0}}),
        Line(points={{-90.0,-78.0},{82.0,-78.0}},
          color={192,192,192}),
        Polygon(lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid,
          points={{90.0,-78.0},{68.0,-70.0},{68.0,-86.0},{90.0,-78.0}}),
        Text(lineColor={192,192,192},
          extent={{-66.0,52.0},{88.0,90.0}},
          textString="%order"),
        Text(fillPattern=FillPattern.Solid,
          extent={{-138.0,-140.0},{162.0,-110.0}},
          textString="f_cut=%f_cut"),
        Rectangle(lineColor={160,160,164},
          fillColor={255,255,255},
          fillPattern=FillPattern.Backward,
          extent={{-80.0,-78.0},{22.0,10.0}}),
        Line(origin = {3.333,-6.667}, points = {{-83.333,34.667},{24.667,34.667},{42.667,-71.333}}, color = {0,0,127}, smooth = Smooth.Bezier)}),
      Documentation(info="<html>

<p>
This blocks models various types of filters:
</p>

<blockquote>
<b>low pass, high pass, band pass, and band stop filters</b>
</blockquote>

<p>
using various filter characteristics:
</p>

<blockquote>
<b>CriticalDamping, Bessel, Butterworth, Chebyshev Type I filters</b>
</blockquote>

<p>
By default, a filter block is initialized in <b>steady-state</b>, in order to
avoid unwanted oscillations at the beginning. In special cases, it might be
useful to select one of the other initialization options under tab
\"Advanced\".
</p>

<p>
Typical frequency responses for the 4 supported low pass filter types
are shown in the next figure:
</p>

<blockquote>
<img src=\"modelica://Modelica/Resources/Images/Blocks/LowPassOrder4Filters.png\"
     alt=\"LowPassOrder4Filters.png\">
</blockquote>

<p>
The step responses of the same low pass filters are shown in the next figure,
starting from a steady state initial filter with initial input = 0.2:
</p>

<blockquote>
<img src=\"modelica://Modelica/Resources/Images/Blocks/LowPassOrder4FiltersStepResponse.png\"
     alt=\"LowPassOrder4FiltersStepResponse.png\">
</blockquote>

<p>
Obviously, the frequency responses give a somewhat wrong impression
of the filter characteristics: Although Butterworth and Chebyshev
filters have a significantly steeper magnitude as the
CriticalDamping and Bessel filters, the step responses of
the latter ones are much better. This means for example, that
a CriticalDamping or a Bessel filter should be selected,
if a filter is mainly used to make a non-linear inverse model
realizable.
</p>

<p>
Typical frequency responses for the 4 supported high pass filter types
are shown in the next figure:
</p>

<blockquote>
<img src=\"modelica://Modelica/Resources/Images/Blocks/HighPassOrder4Filters.png\"
     alt=\"HighPassOrder4Filters.png\">
</blockquote>

<p>
The corresponding step responses of these high pass filters are
shown in the next figure:
</p>
<blockquote>
<img src=\"modelica://Modelica/Resources/Images/Blocks/HighPassOrder4FiltersStepResponse.png\"
     alt=\"HighPassOrder4FiltersStepResponse.png\">
</blockquote>

<p>
All filters are available in <b>normalized</b> (default) and non-normalized form.
In the normalized form, the amplitude of the filter transfer function
at the cut-off frequency f_cut is -3 dB (= 10^(-3/20) = 0.70794..).
Note, when comparing the filters of this function with other software systems,
the setting of \"normalized\" has to be selected appropriately. For example, the signal processing
toolbox of MATLAB provides the filters in non-normalized form and
therefore a comparison makes only sense, if normalized = <b>false</b>
is set. A normalized filter is usually better suited for applications,
since filters of different orders are \"comparable\",
whereas non-normalized filters usually require to adapt the
cut-off frequency, when the order of the filter is changed.
See a comparison of \"normalized\" and \"non-normalized\" filters at hand of
CriticalDamping filters of order 1,2,3:
</p>

<blockquote>
<img src=\"modelica://Modelica/Resources/Images/Blocks/CriticalDampingNormalized.png\"
     alt=\"CriticalDampingNormalized.png\">
</blockquote>

<blockquote>
<img src=\"modelica://Modelica/Resources/Images/Blocks/CriticalDampingNonNormalized.png\"
     alt=\"CriticalDampingNonNormalized.png\">
</blockquote>

<h4>Implementation</h4>

<p>
The filters are implemented in the following, reliable way:
</p>

<ol>
<li> A prototype low pass filter with a cut-off angular frequency of 1 rad/s is constructed
     from the desired analogFilter and the desired normalization.</li>

<li> This prototype low pass filter is transformed to the desired filterType and the
     desired cut-off frequency f_cut using a transformation on the Laplace variable \"s\".</li>

<li> The resulting first and second order transfer functions are implemented in
     state space form, using the \"eigen value\" representation of a transfer function:
     <pre>

  // second order block with eigen values: a +/- jb
  <b>der</b>(x1) = a*x1 - b*x2 + (a^2 + b^2)/b*u;
  <b>der</b>(x2) = b*x1 + a*x2;
       y  = x2;
     </pre>
     The dc-gain from the input to the output of this block is one and the selected
     states are in the order of the input (if \"u\" is in the order of \"one\", then the
     states are also in the order of \"one\"). In the \"Advanced\" tab, a \"nominal\" value for
     the input \"u\" can be given. If appropriately selected, the states are in the order of \"one\" and
     then step-size control is always appropriate.</li>
</ol>

<h4>References</h4>

<dl>
<dt>Tietze U., and Schenk C. (2002):</dt>
<dd> <b>Halbleiter-Schaltungstechnik</b>.
     Springer Verlag, 12. Auflage, pp. 815-852.</dd>
</dl>

</html>", revisions="<html>
<dl>
  <dt><b>Main Author:</b></dt>
  <dd><a href=\"http://www.robotic.dlr.de/Martin.Otter/\">Martin Otter</a>,
      DLR Oberpfaffenhofen.</dd>
</dl>

<h4>Acknowledgement</h4>

<p>
The development of this block was partially funded by BMBF within the
     <a href=\"http://www.eurosyslib.com/\">ITEA2 EUROSYSLIB</a>
      project.
</p>

</html>"));
  end Filter;

  package Internal
    "Internal utility functions and blocks that should not be directly utilized by the user"
      extends Modelica.Icons.InternalPackage;
    package Filter
      "Internal utility functions for filters that should not be directly used"
        extends Modelica.Icons.InternalPackage;
      package base
        "Prototype low pass filters with cut-off frequency of 1 rad/s (other filters are derived by transformation from these base filters)"
          extends Modelica.Icons.InternalPackage;
      function CriticalDamping
          "Return base filter coefficients of CriticalDamping filter (= low pass filter with w_cut = 1 rad/s)"
        extends Modelica.Icons.Function;

        input Integer order(min=1) "Order of filter";
        input Boolean normalized=true
            "= true, if amplitude at f_cut = -3db, otherwise unmodified filter";

        output Real cr[order] "Coefficients of real poles";
        protected
        Real alpha=1.0 "Frequency correction factor";
        Real alpha2 "= alpha*alpha";
        Real den1[order]
            "[p] coefficients of denominator first order polynomials (a*p + 1)";
        Real den2[0,2]
            "[p^2, p] coefficients of denominator second order polynomials (b*p^2 + a*p + 1)";
        Real c0[0] "Coefficients of s^0 term if conjugate complex pole";
        Real c1[0] "Coefficients of s^1 term if conjugate complex pole";
      algorithm
        if normalized then
           // alpha := sqrt(2^(1/order) - 1);
           alpha := sqrt(10^(3/10/order)-1);
        else
           alpha := 1.0;
        end if;

        for i in 1:order loop
           den1[i] := alpha;
        end for;

        // Determine polynomials with highest power of s equal to one
          (cr,c0,c1) :=
            Modelica.Blocks.Continuous.Internal.Filter.Utilities.toHighestPowerOne(
            den1, den2);
      end CriticalDamping;

      function Bessel
          "Return base filter coefficients of Bessel filter (= low pass filter with w_cut = 1 rad/s)"
        extends Modelica.Icons.Function;

        input Integer order(min=1) "Order of filter";
        input Boolean normalized=true
            "= true, if amplitude at f_cut = -3db, otherwise unmodified filter";

        output Real cr[mod(order, 2)] "Coefficient of real pole";
        output Real c0[integer(order/2)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[integer(order/2)]
            "Coefficients of s^1 term if conjugate complex pole";
        protected
        Real alpha=1.0 "Frequency correction factor";
        Real alpha2 "= alpha*alpha";
        Real den1[size(cr,1)]
            "[p] coefficients of denominator first order polynomials (a*p + 1)";
        Real den2[size(c0, 1),2]
            "[p^2, p] coefficients of denominator second order polynomials (b*p^2 + a*p + 1)";
      algorithm
          (den1,den2,alpha) :=
            Modelica.Blocks.Continuous.Internal.Filter.Utilities.BesselBaseCoefficients(
            order);
        if not normalized then
           alpha2 := alpha*alpha;
           for i in 1:size(c0, 1) loop
             den2[i, 1] := den2[i, 1]*alpha2;
             den2[i, 2] := den2[i, 2]*alpha;
           end for;
           if size(cr,1) == 1 then
             den1[1] := den1[1]*alpha;
           end if;
           end if;

        // Determine polynomials with highest power of s equal to one
          (cr,c0,c1) :=
            Modelica.Blocks.Continuous.Internal.Filter.Utilities.toHighestPowerOne(
            den1, den2);
      end Bessel;

      function Butterworth
          "Return base filter coefficients of Butterworth filter (= low pass filter with w_cut = 1 rad/s)"
        import Modelica.Constants.pi;
        extends Modelica.Icons.Function;

        input Integer order(min=1) "Order of filter";
        input Boolean normalized=true
            "= true, if amplitude at f_cut = -3db, otherwise unmodified filter";

        output Real cr[mod(order, 2)] "Coefficient of real pole";
        output Real c0[integer(order/2)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[integer(order/2)]
            "Coefficients of s^1 term if conjugate complex pole";
        protected
        Real alpha=1.0 "Frequency correction factor";
        Real alpha2 "= alpha*alpha";
        Real den1[size(cr,1)]
            "[p] coefficients of denominator first order polynomials (a*p + 1)";
        Real den2[size(c0, 1),2]
            "[p^2, p] coefficients of denominator second order polynomials (b*p^2 + a*p + 1)";
      algorithm
        for i in 1:size(c0, 1) loop
          den2[i, 1] := 1.0;
          den2[i, 2] := -2*Modelica.Math.cos(pi*(0.5 + (i - 0.5)/order));
        end for;
        if size(cr,1) == 1 then
          den1[1] := 1.0;
        end if;

        /* Transformation of filter transfer function with "new(p) = alpha*p"
     in order that the filter transfer function has an amplitude of
     -3 db at the cutoff frequency
  */
        /*
    if normalized then
      alpha := Internal.normalizationFactor(den1, den2);
      alpha2 := alpha*alpha;
      for i in 1:size(c0, 1) loop
        den2[i, 1] := den2[i, 1]*alpha2;
        den2[i, 2] := den2[i, 2]*alpha;
      end for;
      if size(cr,1) == 1 then
        den1[1] := den1[1]*alpha;
      end if;
    end if;
  */

        // Determine polynomials with highest power of s equal to one
          (cr,c0,c1) :=
            Modelica.Blocks.Continuous.Internal.Filter.Utilities.toHighestPowerOne(
            den1, den2);
      end Butterworth;

      function ChebyshevI
          "Return base filter coefficients of Chebyshev I filter (= low pass filter with w_cut = 1 rad/s)"
        import Modelica.Math.asinh;
        import Modelica.Constants.pi;

        extends Modelica.Icons.Function;

        input Integer order(min=1) "Order of filter";
        input Real A_ripple = 0.5 "Pass band ripple in [dB]";
        input Boolean normalized=true
            "= true, if amplitude at f_cut = -3db, otherwise unmodified filter";

        output Real cr[mod(order, 2)] "Coefficient of real pole";
        output Real c0[integer(order/2)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[integer(order/2)]
            "Coefficients of s^1 term if conjugate complex pole";
        protected
        Real epsilon;
        Real fac;
        Real alpha=1.0 "Frequency correction factor";
        Real alpha2 "= alpha*alpha";
        Real den1[size(cr,1)]
            "[p] coefficients of denominator first order polynomials (a*p + 1)";
        Real den2[size(c0, 1),2]
            "[p^2, p] coefficients of denominator second order polynomials (b*p^2 + a*p + 1)";
      algorithm
          epsilon := sqrt(10^(A_ripple/10) - 1);
          fac := asinh(1/epsilon)/order;

          den1 := fill(1/sinh(fac),size(den1,1));
          if size(cr,1) == 0 then
             for i in 1:size(c0, 1) loop
                den2[i,1] :=1/(cosh(fac)^2 - cos((2*i - 1)*pi/(2*order))^2);
                den2[i,2] :=2*den2[i, 1]*sinh(fac)*cos((2*i - 1)*pi/(2*order));
             end for;
          else
             for i in 1:size(c0, 1) loop
                den2[i,1] :=1/(cosh(fac)^2 - cos(i*pi/order)^2);
                den2[i,2] :=2*den2[i, 1]*sinh(fac)*cos(i*pi/order);
             end for;
          end if;

          /* Transformation of filter transfer function with "new(p) = alpha*p"
       in order that the filter transfer function has an amplitude of
       -3 db at the cutoff frequency
    */
          if normalized then
            alpha :=
              Modelica.Blocks.Continuous.Internal.Filter.Utilities.normalizationFactor(
              den1, den2);
            alpha2 := alpha*alpha;
            for i in 1:size(c0, 1) loop
              den2[i, 1] := den2[i, 1]*alpha2;
              den2[i, 2] := den2[i, 2]*alpha;
            end for;
            den1 := den1*alpha;
          end if;

        // Determine polynomials with highest power of s equal to one
          (cr,c0,c1) :=
            Modelica.Blocks.Continuous.Internal.Filter.Utilities.toHighestPowerOne(
            den1, den2);
      end ChebyshevI;
      end base;

      package coefficients "Filter coefficients"
          extends Modelica.Icons.InternalPackage;
      function lowPass
          "Return low pass filter coefficients at given cut-off frequency"
        import Modelica.Constants.pi;
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles";
        input Real c0_in[:]
            "Coefficients of s^0 term if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";
        input Modelica.SIunits.Frequency f_cut "Cut-off frequency";

        output Real cr[size(cr_in,1)] "Coefficient of real pole";
        output Real c0[size(c0_in,1)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";

        protected
        Modelica.SIunits.AngularVelocity w_cut=2*pi*f_cut
            "Cut-off angular frequency";
        Real w_cut2=w_cut*w_cut;

      algorithm
        assert(f_cut > 0, "Cut-off frequency f_cut must be positive");

        /* Change filter coefficients according to transformation new(s) = s/w_cut
     s + cr           -> (s/w) + cr              = (s + w*cr)/w
     s^2 + c1*s + c0  -> (s/w)^2 + c1*(s/w) + c0 = (s^2 + (c1*w)*s + (c0*w^2))/w^2
  */
        cr := w_cut*cr_in;
        c1 := w_cut*c1_in;
        c0 := w_cut2*c0_in;

      end lowPass;

      function highPass
          "Return high pass filter coefficients at given cut-off frequency"
        import Modelica.Constants.pi;
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles";
        input Real c0_in[:]
            "Coefficients of s^0 term if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";
        input Modelica.SIunits.Frequency f_cut "Cut-off frequency";

        output Real cr[size(cr_in,1)] "Coefficient of real pole";
        output Real c0[size(c0_in,1)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";

        protected
        Modelica.SIunits.AngularVelocity w_cut=2*pi*f_cut
            "Cut-off angular frequency";
        Real w_cut2=w_cut*w_cut;

      algorithm
        assert(f_cut > 0, "Cut-off frequency f_cut must be positive");

        /* Change filter coefficients according to transformation: new(s) = 1/s
        1/(s + cr)          -> 1/(1/s + cr)                = (1/cr)*s / (s + (1/cr))
        1/(s^2 + c1*s + c0) -> 1/((1/s)^2 + c1*(1/s) + c0) = (1/c0)*s^2 / (s^2 + (c1/c0)*s + 1/c0)

     Check whether transformed roots are also conjugate complex:
        c0 - c1^2/4 > 0  -> (1/c0) - (c1/c0)^2 / 4
                            = (c0 - c1^2/4) / c0^2 > 0
        It is therefore guaranteed that the roots remain conjugate complex

     Change filter coefficients according to transformation new(s) = s/w_cut
        s + 1/cr                -> (s/w) + 1/cr                   = (s + w/cr)/w
        s^2 + (c1/c0)*s + 1/c0  -> (s/w)^2 + (c1/c0)*(s/w) + 1/c0 = (s^2 + (w*c1/c0)*s + (w^2/c0))/w^2
  */
        for i in 1:size(cr_in,1) loop
           cr[i] := w_cut/cr_in[i];
        end for;

        for i in 1:size(c0_in,1) loop
           c0[i] := w_cut2/c0_in[i];
           c1[i] := w_cut*c1_in[i]/c0_in[i];
        end for;

      end highPass;

      function bandPass
          "Return band pass filter coefficients at given cut-off frequency"
        import Modelica.Constants.pi;
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles";
        input Real c0_in[:]
            "Coefficients of s^0 term if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";
        input Modelica.SIunits.Frequency f_min
            "Band of band pass filter is f_min (A=-3db) .. f_max (A=-3db)";
        input Modelica.SIunits.Frequency f_max "Upper band frequency";

        output Real cr[0] "Coefficient of real pole";
        output Real c0[size(cr_in,1) + 2*size(c0_in,1)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[size(cr_in,1) + 2*size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";
        output Real cn "Numerator coefficient of the PT2 terms";
        protected
        Modelica.SIunits.Frequency f0 = sqrt(f_min*f_max);
        Modelica.SIunits.AngularVelocity w_cut=2*pi*f0
            "Cut-off angular frequency";
        Real w_band = (f_max - f_min) / f0;
        Real w_cut2=w_cut*w_cut;
        Real c;
        Real alpha;
        Integer j;
      algorithm
        assert(f_min > 0 and f_min < f_max, "Band frequencies f_min and f_max are wrong");

          /* The band pass filter is derived from the low pass filter by
       the transformation new(s) = (s + 1/s)/w   (w = w_band = (f_max - f_min)/sqrt(f_max*f_min) )

       1/(s + cr)         -> 1/((s/w + 1/s/w) + cr)
                             = w*s / (s^2 + cr*w*s + 1)

       1/(s^2 + c1*s + c0) -> 1/( (s+1/s)^2/w^2 + c1*(s + 1/s)/w + c0 )
                              = 1 /( ( s^2 + 1/s^2 + 2)/w^2 + (s + 1/s)*c1/w + c0 )
                              = w^2*s^2 / (s^4 + 2*s^2 + 1 + (s^3 + s)*c1*w + c0*w^2*s^2)
                              = w^2*s^2 / (s^4 + c1*w*s^3 + (2+c0*w^2)*s^2 + c1*w*s + 1)

                              Assume the following description with PT2:
                              = w^2*s^2 /( (s^2 + s*(c/alpha) + 1/alpha^2)*
                                           (s^2 + s*(c*alpha) + alpha^2) )
                              = w^2*s^2 / ( s^4 + c*(alpha + 1/alpha)*s^3
                                                + (alpha^2 + 1/alpha^2 + c^2)*s^2
                                                + c*(alpha + 1/alpha)*s + 1 )

                              and therefore:
                                c*(alpha + 1/alpha) = c1*w       -> c = c1*w / (alpha + 1/alpha)
                                                                      = c1*w*alpha/(1+alpha^2)
                                alpha^2 + 1/alpha^2 + c^2 = 2+c0*w^2 -> equation to determine alpha
                                alpha^4 + 1 + c1^2*w^2*alpha^4/(1+alpha^2)^2 = (2+c0*w^2)*alpha^2
                                or z = alpha^2
                                z^2 + c^1^2*w^2*z^2/(1+z)^2 - (2+c0*w^2)*z + 1 = 0

     Check whether roots remain conjugate complex
        c0 - (c1/2)^2 > 0:    1/alpha^2 - (c/alpha)^2/4
                              = 1/alpha^2*(1 - c^2/4)    -> not possible to figure this out

     Afterwards, change filter coefficients according to transformation new(s) = s/w_cut
        w_band*s/(s^2 + c1*s + c0)  -> w_band*(s/w)/((s/w)^2 + c1*(s/w) + c0 =
                                       (w_band/w)*s/(s^2 + (c1*w)*s + (c0*w^2))/w^2) =
                                       (w_band*w)*s/(s^2 + (c1*w)*s + (c0*w^2))
    */
          for i in 1:size(cr_in,1) loop
             c1[i] := w_cut*cr_in[i]*w_band;
             c0[i] := w_cut2;
          end for;

          for i in 1:size(c1_in,1) loop
            alpha :=
              Modelica.Blocks.Continuous.Internal.Filter.Utilities.bandPassAlpha(
                    c1_in[i],
                    c0_in[i],
                    w_band);
             c       := c1_in[i]*w_band / (alpha + 1/alpha);
             j       := size(cr_in,1) + 2*i - 1;
             c1[j]   := w_cut*c/alpha;
             c1[j+1] := w_cut*c*alpha;
             c0[j]   := w_cut2/alpha^2;
             c0[j+1] := w_cut2*alpha^2;
          end for;

          cn :=w_band*w_cut;

      end bandPass;

      function bandStop
          "Return band stop filter coefficients at given cut-off frequency"
        import Modelica.Constants.pi;
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles";
        input Real c0_in[:]
            "Coefficients of s^0 term if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";
        input Modelica.SIunits.Frequency f_min
            "Band of band stop filter is f_min (A=-3db) .. f_max (A=-3db)";
        input Modelica.SIunits.Frequency f_max "Upper band frequency";

        output Real cr[0] "Coefficient of real pole";
        output Real c0[size(cr_in,1) + 2*size(c0_in,1)]
            "Coefficients of s^0 term if conjugate complex pole";
        output Real c1[size(cr_in,1) + 2*size(c0_in,1)]
            "Coefficients of s^1 term if conjugate complex pole";
        protected
        Modelica.SIunits.Frequency f0 = sqrt(f_min*f_max);
        Modelica.SIunits.AngularVelocity w_cut=2*pi*f0
            "Cut-off angular frequency";
        Real w_band = (f_max - f_min) / f0;
        Real w_cut2=w_cut*w_cut;
        Real c;
        Real ww;
        Real alpha;
        Integer j;
      algorithm
        assert(f_min > 0 and f_min < f_max, "Band frequencies f_min and f_max are wrong");

          /* The band pass filter is derived from the low pass filter by
       the transformation new(s) = (s + 1/s)/w   (w = w_band = (f_max - f_min)/sqrt(f_max*f_min) )

       1/(s + cr)         -> 1/((s/w + 1/s/w) + cr)
                             = w*s / (s^2 + cr*w*s + 1)

       1/(s^2 + c1*s + c0) -> 1/( (s+1/s)^2/w^2 + c1*(s + 1/s)/w + c0 )
                              = 1 /( ( s^2 + 1/s^2 + 2)/w^2 + (s + 1/s)*c1/w + c0 )
                              = w^2*s^2 / (s^4 + 2*s^2 + 1 + (s^3 + s)*c1*w + c0*w^2*s^2)
                              = w^2*s^2 / (s^4 + c1*w*s^3 + (2+c0*w^2)*s^2 + c1*w*s + 1)

                              Assume the following description with PT2:
                              = w^2*s^2 /( (s^2 + s*(c/alpha) + 1/alpha^2)*
                                           (s^2 + s*(c*alpha) + alpha^2) )
                              = w^2*s^2 / ( s^4 + c*(alpha + 1/alpha)*s^3
                                                + (alpha^2 + 1/alpha^2 + c^2)*s^2
                                                + c*(alpha + 1/alpha)*s + 1 )

                              and therefore:
                                c*(alpha + 1/alpha) = c1*w       -> c = c1*w / (alpha + 1/alpha)
                                                                      = c1*w*alpha/(1+alpha^2)
                                alpha^2 + 1/alpha^2 + c^2 = 2+c0*w^2 -> equation to determine alpha
                                alpha^4 + 1 + c1^2*w^2*alpha^4/(1+alpha^2)^2 = (2+c0*w^2)*alpha^2
                                or z = alpha^2
                                z^2 + c^1^2*w^2*z^2/(1+z)^2 - (2+c0*w^2)*z + 1 = 0

       The band stop filter is derived from the low pass filter by
       the transformation new(s) = w/( (s + 1/s) )   (w = w_band = (f_max - f_min)/sqrt(f_max*f_min) )

       cr/(s + cr)         -> 1/(( w/(s + 1/s) ) + cr)
                              = (s^2 + 1) / (s^2 + (w/cr)*s + 1)

       c0/(s^2 + c1*s + c0) -> c0/( w^2/(s + 1/s)^2 + c1*w/(s + 1/s) + c0 )
                               = c0*(s^2 + 1)^2 / (s^4 + c1*w*s^3/c0 + (2+w^2/b)*s^2 + c1*w*s/c0 + 1)

                               Assume the following description with PT2:
                               = c0*(s^2 + 1)^2 / ( (s^2 + s*(c/alpha) + 1/alpha^2)*
                                                    (s^2 + s*(c*alpha) + alpha^2) )
                               = c0*(s^2 + 1)^2 / (  s^4 + c*(alpha + 1/alpha)*s^3
                                                         + (alpha^2 + 1/alpha^2 + c^2)*s^2
                                                         + c*(alpha + 1/alpha)*p + 1 )

                            and therefore:
                              c*(alpha + 1/alpha) = c1*w/b         -> c = c1*w/(c0*(alpha + 1/alpha))
                              alpha^2 + 1/alpha^2 + c^2 = 2+w^2/c0 -> equation to determine alpha
                              alpha^4 + 1 + (c1*w/c0*alpha^2)^2/(1+alpha^2)^2 = (2+w^2/c0)*alpha^2
                              or z = alpha^2
                              z^2 + (c1*w/c0*z)^2/(1+z)^2 - (2+w^2/c0)*z + 1 = 0

                            same as:  ww = w/c0
                              z^2 + (c1*ww*z)^2/(1+z)^2 - (2+c0*ww)*z + 1 = 0  -> same equation as for BandPass

     Afterwards, change filter coefficients according to transformation new(s) = s/w_cut
        c0*(s^2+1)(s^2 + c1*s + c0)  -> c0*((s/w)^2 + 1) / ((s/w)^2 + c1*(s/w) + c0 =
                                        c0/w^2*(s^2 + w^2) / (s^2 + (c1*w)*s + (c0*w^2))/w^2) =
                                        (s^2 + c0*w^2) / (s^2 + (c1*w)*s + (c0*w^2))
    */
          for i in 1:size(cr_in,1) loop
             c1[i] := w_cut*w_band/cr_in[i];
             c0[i] := w_cut2;
          end for;

          for i in 1:size(c1_in,1) loop
             ww      := w_band/c0_in[i];
            alpha :=
              Modelica.Blocks.Continuous.Internal.Filter.Utilities.bandPassAlpha(
                    c1_in[i],
                    c0_in[i],
                    ww);
             c       := c1_in[i]*ww / (alpha + 1/alpha);
             j       := size(cr_in,1) + 2*i - 1;
             c1[j]   := w_cut*c/alpha;
             c1[j+1] := w_cut*c*alpha;
             c0[j]   := w_cut2/alpha^2;
             c0[j+1] := w_cut2*alpha^2;
          end for;

      end bandStop;
      end coefficients;

      package roots "Filter roots and gain as needed for block implementations"
          extends Modelica.Icons.InternalPackage;
      function lowPass
          "Return low pass filter roots as needed for block for given cut-off frequency"
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles of base filter";
        input Real c0_in[:]
            "Coefficients of s^0 term of base filter if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term of base filter if conjugate complex pole";
        input Modelica.SIunits.Frequency f_cut "Cut-off frequency";

        output Real r[size(cr_in,1)] "Real eigenvalues";
        output Real a[size(c0_in,1)]
            "Real parts of complex conjugate eigenvalues";
        output Real b[size(c0_in,1)]
            "Imaginary parts of complex conjugate eigenvalues";
        output Real ku[size(c0_in,1)] "Input gain";
        protected
        Real c0[size(c0_in,1)];
        Real c1[size(c0_in,1)];
        Real cr[size(cr_in,1)];
      algorithm
        // Get coefficients of low pass filter at f_cut
        (cr, c0, c1) :=coefficients.lowPass(cr_in, c0_in, c1_in, f_cut);

        // Transform coefficients in to root
        for i in 1:size(cr_in,1) loop
          r[i] :=-cr[i];
        end for;

        for i in 1:size(c0_in,1) loop
          a [i] :=-c1[i]/2;
          b [i] :=sqrt(c0[i] - a[i]*a[i]);
          ku[i] :=c0[i]/b[i];
        end for;

        annotation (Documentation(info="<html>

<p>
The goal is to implement the filter in the following form:
</p>

<pre>
  // real pole:
   der(x) = r*x - r*u
       y  = x

  // complex conjugate poles:
  der(x1) = a*x1 - b*x2 + ku*u;
  der(x2) = b*x1 + a*x2;
       y  = x2;

            ku = (a^2 + b^2)/b
</pre>
<p>
This representation has the following transfer function:
</p>
<pre>
// real pole:
    s*y = r*y - r*u
  or
    (s-r)*y = -r*u
  or
    y = -r/(s-r)*u

  comparing coefficients with
    y = cr/(s + cr)*u  ->  r = -cr      // r is the real eigenvalue

// complex conjugate poles
    s*x2 =  a*x2 + b*x1
    s*x1 = -b*x2 + a*x1 + ku*u
  or
    (s-a)*x2               = b*x1  ->  x2 = b/(s-a)*x1
    (s + b^2/(s-a) - a)*x1 = ku*u  ->  (s(s-a) + b^2 - a*(s-a))*x1  = ku*(s-a)*u
                                   ->  (s^2 - 2*a*s + a^2 + b^2)*x1 = ku*(s-a)*u
  or
    x1 = ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
    x2 = b/(s-a)*ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
       = b*ku/(s^2 - 2*a*s + a^2 + b^2)*u
    y  = x2

  comparing coefficients with
    y = c0/(s^2 + c1*s + c0)*u  ->  a  = -c1/2
                                    b  = sqrt(c0 - a^2)
                                    ku = c0/b
                                       = (a^2 + b^2)/b

  comparing with eigenvalue representation:
    (s - (a+jb))*(s - (a-jb)) = s^2 -2*a*s + a^2 + b^2
  shows that:
    a: real part of eigenvalue
    b: imaginary part of eigenvalue

  time -> infinity:
    y(s=0) = x2(s=0) = 1
             x1(s=0) = -ku*a/(a^2 + b^2)*u
                     = -(a/b)*u
</pre>

</html>"));
      end lowPass;

      function highPass
          "Return high pass filter roots as needed for block for given cut-off frequency"
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles of base filter";
        input Real c0_in[:]
            "Coefficients of s^0 term of base filter if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term of base filter if conjugate complex pole";
        input Modelica.SIunits.Frequency f_cut "Cut-off frequency";

        output Real r[size(cr_in,1)] "Real eigenvalues";
        output Real a[size(c0_in,1)]
            "Real parts of complex conjugate eigenvalues";
        output Real b[size(c0_in,1)]
            "Imaginary parts of complex conjugate eigenvalues";
        output Real ku[size(c0_in,1)] "Gains of input terms";
        output Real k1[size(c0_in,1)] "Gains of y = k1*x1 + k2*x + u";
        output Real k2[size(c0_in,1)] "Gains of y = k1*x1 + k2*x + u";
        protected
        Real c0[size(c0_in,1)];
        Real c1[size(c0_in,1)];
        Real cr[size(cr_in,1)];
        Real ba2;
      algorithm
        // Get coefficients of high pass filter at f_cut
        (cr, c0, c1) :=coefficients.highPass(cr_in, c0_in, c1_in, f_cut);

        // Transform coefficients in to roots
        for i in 1:size(cr_in,1) loop
          r[i] :=-cr[i];
        end for;

        for i in 1:size(c0_in,1) loop
          a[i]  := -c1[i]/2;
          b[i]  := sqrt(c0[i] - a[i]*a[i]);
          ku[i] := c0[i]/b[i];
          k1[i] := 2*a[i]/ku[i];
          ba2   := (b[i]/a[i])^2;
          k2[i] := (1-ba2)/(1+ba2);
        end for;

        annotation (Documentation(info="<html>

<p>
The goal is to implement the filter in the following form:
</p>

<pre>
  // real pole:
   der(x) = r*x - r*u
       y  = -x + u

  // complex conjugate poles:
  der(x1) = a*x1 - b*x2 + ku*u;
  der(x2) = b*x1 + a*x2;
       y  = k1*x1 + k2*x2 + u;

            ku = (a^2 + b^2)/b
            k1 = 2*a/ku
            k2 = (a^2 - b^2) / (b*ku)
               = (a^2 - b^2) / (a^2 + b^2)
               = (1 - (b/a)^2) / (1 + (b/a)^2)

</pre>
<p>
This representation has the following transfer function:
</p>
<pre>
// real pole:
    s*x = r*x - r*u
  or
    (s-r)*x = -r*u   -> x = -r/(s-r)*u
  or
    y = r/(s-r)*u + (s-r)/(s-r)*u
      = (r+s-r)/(s-r)*u
      = s/(s-r)*u

  comparing coefficients with
    y = s/(s + cr)*u  ->  r = -cr      // r is the real eigenvalue

// complex conjugate poles
    s*x2 =  a*x2 + b*x1
    s*x1 = -b*x2 + a*x1 + ku*u
  or
    (s-a)*x2               = b*x1  ->  x2 = b/(s-a)*x1
    (s + b^2/(s-a) - a)*x1 = ku*u  ->  (s(s-a) + b^2 - a*(s-a))*x1  = ku*(s-a)*u
                                   ->  (s^2 - 2*a*s + a^2 + b^2)*x1 = ku*(s-a)*u
  or
    x1 = ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
    x2 = b/(s-a)*ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
       = b*ku/(s^2 - 2*a*s + a^2 + b^2)*u
    y  = k1*x1 + k2*x2 + u
       = (k1*ku*(s-a) + k2*b*ku +  s^2 - 2*a*s + a^2 + b^2) /
         (s^2 - 2*a*s + a^2 + b^2)*u
       = (s^2 + (k1*ku - 2*a)*s + k2*b*ku - k1*ku*a + a^2 + b^2) /
         (s^2 - 2*a*s + a^2 + b^2)*u
       = (s^2 + (2*a-2*a)*s + a^2 - b^2 - 2*a^2 + a^2 + b^2) /
         (s^2 - 2*a*s + a^2 + b^2)*u
       = s^2 / (s^2 - 2*a*s + a^2 + b^2)*u

  comparing coefficients with
    y = s^2/(s^2 + c1*s + c0)*u  ->  a = -c1/2
                                     b = sqrt(c0 - a^2)

  comparing with eigenvalue representation:
    (s - (a+jb))*(s - (a-jb)) = s^2 -2*a*s + a^2 + b^2
  shows that:
    a: real part of eigenvalue
    b: imaginary part of eigenvalue
</pre>

</html>"));
      end highPass;

      function bandPass
          "Return band pass filter roots as needed for block for given cut-off frequency"
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles of base filter";
        input Real c0_in[:]
            "Coefficients of s^0 term of base filter if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term of base filter if conjugate complex pole";
        input Modelica.SIunits.Frequency f_min
            "Band of band pass filter is f_min (A=-3db) .. f_max (A=-3db)";
        input Modelica.SIunits.Frequency f_max "Upper band frequency";

        output Real a[size(cr_in,1) + 2*size(c0_in,1)]
            "Real parts of complex conjugate eigenvalues";
        output Real b[size(cr_in,1) + 2*size(c0_in,1)]
            "Imaginary parts of complex conjugate eigenvalues";
        output Real ku[size(cr_in,1) + 2*size(c0_in,1)] "Gains of input terms";
        output Real k1[size(cr_in,1) + 2*size(c0_in,1)]
            "Gains of y = k1*x1 + k2*x";
        output Real k2[size(cr_in,1) + 2*size(c0_in,1)]
            "Gains of y = k1*x1 + k2*x";
        protected
        Real cr[0];
        Real c0[size(a,1)];
        Real c1[size(a,1)];
        Real cn;
        Real bb;
      algorithm
        // Get coefficients of band pass filter at f_cut
        (cr, c0, c1, cn) :=coefficients.bandPass(cr_in, c0_in, c1_in, f_min, f_max);

        // Transform coefficients in to roots
        for i in 1:size(a,1) loop
          a[i]  := -c1[i]/2;
          bb    := c0[i] - a[i]*a[i];
          assert(bb >= 0, "\nNot possible to use band pass filter, since transformation results in\n"+
                          "system that does not have conjugate complex poles.\n" +
                          "Try to use another analog filter for the band pass.\n");
          b[i]  := sqrt(bb);
          ku[i] := c0[i]/b[i];
          k1[i] := cn/ku[i];
          k2[i] := cn*a[i]/(b[i]*ku[i]);
        end for;

        annotation (Documentation(info="<html>

<p>
The goal is to implement the filter in the following form:
</p>

<pre>
  // complex conjugate poles:
  der(x1) = a*x1 - b*x2 + ku*u;
  der(x2) = b*x1 + a*x2;
       y  = k1*x1 + k2*x2;

            ku = (a^2 + b^2)/b
            k1 = cn/ku
            k2 = cn*a/(b*ku)
</pre>
<p>
This representation has the following transfer function:
</p>
<pre>
// complex conjugate poles
    s*x2 =  a*x2 + b*x1
    s*x1 = -b*x2 + a*x1 + ku*u
  or
    (s-a)*x2               = b*x1  ->  x2 = b/(s-a)*x1
    (s + b^2/(s-a) - a)*x1 = ku*u  ->  (s(s-a) + b^2 - a*(s-a))*x1  = ku*(s-a)*u
                                   ->  (s^2 - 2*a*s + a^2 + b^2)*x1 = ku*(s-a)*u
  or
    x1 = ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
    x2 = b/(s-a)*ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
       = b*ku/(s^2 - 2*a*s + a^2 + b^2)*u
    y  = k1*x1 + k2*x2
       = (k1*ku*(s-a) + k2*b*ku) / (s^2 - 2*a*s + a^2 + b^2)*u
       = (k1*ku*s + k2*b*ku - k1*ku*a) / (s^2 - 2*a*s + a^2 + b^2)*u
       = (cn*s + cn*a - cn*a) / (s^2 - 2*a*s + a^2 + b^2)*u
       = cn*s / (s^2 - 2*a*s + a^2 + b^2)*u

  comparing coefficients with
    y = cn*s / (s^2 + c1*s + c0)*u  ->  a = -c1/2
                                        b = sqrt(c0 - a^2)

  comparing with eigenvalue representation:
    (s - (a+jb))*(s - (a-jb)) = s^2 -2*a*s + a^2 + b^2
  shows that:
    a: real part of eigenvalue
    b: imaginary part of eigenvalue
</pre>

</html>"));
      end bandPass;

      function bandStop
          "Return band stop filter roots as needed for block for given cut-off frequency"
        extends Modelica.Icons.Function;

        input Real cr_in[:] "Coefficients of real poles of base filter";
        input Real c0_in[:]
            "Coefficients of s^0 term of base filter if conjugate complex pole";
        input Real c1_in[size(c0_in,1)]
            "Coefficients of s^1 term of base filter if conjugate complex pole";
        input Modelica.SIunits.Frequency f_min
            "Band of band stop filter is f_min (A=-3db) .. f_max (A=-3db)";
        input Modelica.SIunits.Frequency f_max "Upper band frequency";

        output Real a[size(cr_in,1) + 2*size(c0_in,1)]
            "Real parts of complex conjugate eigenvalues";
        output Real b[size(cr_in,1) + 2*size(c0_in,1)]
            "Imaginary parts of complex conjugate eigenvalues";
        output Real ku[size(cr_in,1) + 2*size(c0_in,1)] "Gains of input terms";
        output Real k1[size(cr_in,1) + 2*size(c0_in,1)]
            "Gains of y = k1*x1 + k2*x";
        output Real k2[size(cr_in,1) + 2*size(c0_in,1)]
            "Gains of y = k1*x1 + k2*x";
        protected
        Real cr[0];
        Real c0[size(a,1)];
        Real c1[size(a,1)];
        Real cn;
        Real bb;
      algorithm
        // Get coefficients of band stop filter at f_cut
        (cr, c0, c1) :=coefficients.bandStop(cr_in, c0_in, c1_in, f_min, f_max);

        // Transform coefficients in to roots
        for i in 1:size(a,1) loop
          a[i]  := -c1[i]/2;
          bb    := c0[i] - a[i]*a[i];
          assert(bb >= 0, "\nNot possible to use band stop filter, since transformation results in\n"+
                          "system that does not have conjugate complex poles.\n" +
                          "Try to use another analog filter for the band stop filter.\n");
          b[i]  := sqrt(bb);
          ku[i] := c0[i]/b[i];
          k1[i] := 2*a[i]/ku[i];
          k2[i] := (c0[i] + a[i]^2 - b[i]^2)/(b[i]*ku[i]);
        end for;

        annotation (Documentation(info="<html>

<p>
The goal is to implement the filter in the following form:
</p>

<pre>
  // complex conjugate poles:
  der(x1) = a*x1 - b*x2 + ku*u;
  der(x2) = b*x1 + a*x2;
       y  = k1*x1 + k2*x2 + u;

            ku = (a^2 + b^2)/b
            k1 = 2*a/ku
            k2 = (c0 + a^2 - b^2)/(b*ku)
</pre>
<p>
This representation has the following transfer function:
</p>
<pre>
// complex conjugate poles
    s*x2 =  a*x2 + b*x1
    s*x1 = -b*x2 + a*x1 + ku*u
  or
    (s-a)*x2               = b*x1  ->  x2 = b/(s-a)*x1
    (s + b^2/(s-a) - a)*x1 = ku*u  ->  (s(s-a) + b^2 - a*(s-a))*x1  = ku*(s-a)*u
                                   ->  (s^2 - 2*a*s + a^2 + b^2)*x1 = ku*(s-a)*u
  or
    x1 = ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
    x2 = b/(s-a)*ku*(s-a)/(s^2 - 2*a*s + a^2 + b^2)*u
       = b*ku/(s^2 - 2*a*s + a^2 + b^2)*u
    y  = k1*x1 + k2*x2 + u
       = (k1*ku*(s-a) + k2*b*ku + s^2 - 2*a*s + a^2 + b^2) / (s^2 - 2*a*s + a^2 + b^2)*u
       = (s^2 + (k1*ku-2*a)*s + k2*b*ku - k1*ku*a + a^2 + b^2) / (s^2 - 2*a*s + a^2 + b^2)*u
       = (s^2 + c0 + a^2 - b^2 - 2*a^2 + a^2 + b^2) / (s^2 - 2*a*s + a^2 + b^2)*u
       = (s^2 + c0) / (s^2 - 2*a*s + a^2 + b^2)*u

  comparing coefficients with
    y = (s^2 + c0) / (s^2 + c1*s + c0)*u  ->  a = -c1/2
                                              b = sqrt(c0 - a^2)

  comparing with eigenvalue representation:
    (s - (a+jb))*(s - (a-jb)) = s^2 -2*a*s + a^2 + b^2
  shows that:
    a: real part of eigenvalue
    b: imaginary part of eigenvalue
</pre>

</html>"));
      end bandStop;
      end roots;

      package Utilities "Utility functions for filter computations"
          extends Modelica.Icons.InternalPackage;
        function BesselBaseCoefficients
          "Return coefficients of normalized low pass Bessel filter (= gain at cut-off frequency 1 rad/s is decreased 3dB)"
          extends Modelica.Icons.Function;

          import Modelica.Utilities.Streams;
          input Integer order "Order of filter in the range 1..41";
          output Real c1[mod(order, 2)]
            "[p] coefficients of Bessel denominator polynomials (a*p + 1)";
          output Real c2[integer(order/2),2]
            "[p^2, p] coefficients of Bessel denominator polynomials (b2*p^2 + b1*p + 1)";
          output Real alpha "Normalization factor";
        algorithm
          if order == 1 then
            alpha := 1.002377293007601;
            c1[1] := 0.9976283451109835;
          elseif order == 2 then
            alpha := 0.7356641785819585;
            c2[1, 1] := 0.6159132201783791;
            c2[1, 2] := 1.359315879600889;
          elseif order == 3 then
            alpha := 0.5704770156982642;
            c1[1] := 0.7548574865985343;
            c2[1, 1] := 0.4756958028827457;
            c2[1, 2] := 0.9980615136104388;
          elseif order == 4 then
            alpha := 0.4737978580281427;
            c2[1, 1] := 0.4873729247240677;
            c2[1, 2] := 1.337564170455762;
            c2[2, 1] := 0.3877724315741958;
            c2[2, 2] := 0.7730405590839861;
          elseif order == 5 then
            alpha := 0.4126226974763408;
            c1[1] := 0.6645723262620757;
            c2[1, 1] := 0.4115231900614016;
            c2[1, 2] := 1.138349926728708;
            c2[2, 1] := 0.3234938702877912;
            c2[2, 2] := 0.6205992985771313;
          elseif order == 6 then
            alpha := 0.3705098000736233;
            c2[1, 1] := 0.3874508649098960;
            c2[1, 2] := 1.219740879520741;
            c2[2, 1] := 0.3493298843155746;
            c2[2, 2] := 0.9670265529381365;
            c2[3, 1] := 0.2747419229514599;
            c2[3, 2] := 0.5122165075105700;
          elseif order == 7 then
            alpha := 0.3393452623586350;
            c1[1] := 0.5927147125821412;
            c2[1, 1] := 0.3383379423919174;
            c2[1, 2] := 1.092630816438030;
            c2[2, 1] := 0.3001025788696046;
            c2[2, 2] := 0.8289928256598656;
            c2[3, 1] := 0.2372867471539579;
            c2[3, 2] := 0.4325128641920154;
          elseif order == 8 then
            alpha := 0.3150267393795002;
            c2[1, 1] := 0.3151115975207653;
            c2[1, 2] := 1.109403015460190;
            c2[2, 1] := 0.2969344839572762;
            c2[2, 2] := 0.9737455812222699;
            c2[3, 1] := 0.2612545921889538;
            c2[3, 2] := 0.7190394712068573;
            c2[4, 1] := 0.2080523342974281;
            c2[4, 2] := 0.3721456473047434;
          elseif order == 9 then
            alpha := 0.2953310177184124;
            c1[1] := 0.5377196679501422;
            c2[1, 1] := 0.2824689124281034;
            c2[1, 2] := 1.022646191567475;
            c2[2, 1] := 0.2626824161383468;
            c2[2, 2] := 0.8695626454762596;
            c2[3, 1] := 0.2302781917677917;
            c2[3, 2] := 0.6309047553448520;
            c2[4, 1] := 0.1847991729757028;
            c2[4, 2] := 0.3251978031287202;
          elseif order == 10 then
            alpha := 0.2789426890619463;
            c2[1, 1] := 0.2640769908255582;
            c2[1, 2] := 1.019788132875305;
            c2[2, 1] := 0.2540802639216947;
            c2[2, 2] := 0.9377020417760623;
            c2[3, 1] := 0.2343577229427963;
            c2[3, 2] := 0.7802229808216112;
            c2[4, 1] := 0.2052193139338624;
            c2[4, 2] := 0.5594176813008133;
            c2[5, 1] := 0.1659546953748916;
            c2[5, 2] := 0.2878349616233292;
          elseif order == 11 then
            alpha := 0.2650227766037203;
            c1[1] := 0.4950265498954191;
            c2[1, 1] := 0.2411858478546218;
            c2[1, 2] := 0.9567800996387417;
            c2[2, 1] := 0.2296849355380925;
            c2[2, 2] := 0.8592523717113126;
            c2[3, 1] := 0.2107851705677406;
            c2[3, 2] := 0.7040216048898129;
            c2[4, 1] := 0.1846461385164021;
            c2[4, 2] := 0.5006729207276717;
            c2[5, 1] := 0.1504217970817433;
            c2[5, 2] := 0.2575070491320295;
          elseif order == 12 then
            alpha := 0.2530051198547209;
            c2[1, 1] := 0.2268294941204543;
            c2[1, 2] := 0.9473116570034053;
            c2[2, 1] := 0.2207657387793729;
            c2[2, 2] := 0.8933728946287606;
            c2[3, 1] := 0.2087600700376653;
            c2[3, 2] := 0.7886236252756229;
            c2[4, 1] := 0.1909959101492760;
            c2[4, 2] := 0.6389263649257017;
            c2[5, 1] := 0.1675208146048472;
            c2[5, 2] := 0.4517847275162215;
            c2[6, 1] := 0.1374257286372761;
            c2[6, 2] := 0.2324699157474680;
          elseif order == 13 then
            alpha := 0.2424910397561007;
            c1[1] := 0.4608848369928040;
            c2[1, 1] := 0.2099813050274780;
            c2[1, 2] := 0.8992478823790660;
            c2[2, 1] := 0.2027250423101359;
            c2[2, 2] := 0.8328117484224146;
            c2[3, 1] := 0.1907635894058731;
            c2[3, 2] := 0.7257379204691213;
            c2[4, 1] := 0.1742280397887686;
            c2[4, 2] := 0.5830640944868014;
            c2[5, 1] := 0.1530858190490478;
            c2[5, 2] := 0.4106192089751885;
            c2[6, 1] := 0.1264090712880446;
            c2[6, 2] := 0.2114980230156001;
          elseif order == 14 then
            alpha := 0.2331902368695848;
            c2[1, 1] := 0.1986162311411235;
            c2[1, 2] := 0.8876961808055535;
            c2[2, 1] := 0.1946683341271615;
            c2[2, 2] := 0.8500754229171967;
            c2[3, 1] := 0.1868331332895056;
            c2[3, 2] := 0.7764629313723603;
            c2[4, 1] := 0.1752118757862992;
            c2[4, 2] := 0.6699720402924552;
            c2[5, 1] := 0.1598906457908402;
            c2[5, 2] := 0.5348446712848934;
            c2[6, 1] := 0.1407810153019944;
            c2[6, 2] := 0.3755841316563539;
            c2[7, 1] := 0.1169627966707339;
            c2[7, 2] := 0.1937088226304455;
          elseif order == 15 then
            alpha := 0.2248854870552422;
            c1[1] := 0.4328492272335646;
            c2[1, 1] := 0.1857292591004588;
            c2[1, 2] := 0.8496337061962563;
            c2[2, 1] := 0.1808644178280136;
            c2[2, 2] := 0.8020517898136011;
            c2[3, 1] := 0.1728264404199081;
            c2[3, 2] := 0.7247449729331105;
            c2[4, 1] := 0.1616970125901954;
            c2[4, 2] := 0.6205369315943097;
            c2[5, 1] := 0.1475257264578426;
            c2[5, 2] := 0.4929612162355906;
            c2[6, 1] := 0.1301861023357119;
            c2[6, 2] := 0.3454770708040735;
            c2[7, 1] := 0.1087810777120188;
            c2[7, 2] := 0.1784526655428406;
          elseif order == 16 then
            alpha := 0.2174105053474761;
            c2[1, 1] := 0.1765637967473151;
            c2[1, 2] := 0.8377453068635511;
            c2[2, 1] := 0.1738525357503125;
            c2[2, 2] := 0.8102988957433199;
            c2[3, 1] := 0.1684627004613343;
            c2[3, 2] := 0.7563265923413258;
            c2[4, 1] := 0.1604519074815815;
            c2[4, 2] := 0.6776082294687619;
            c2[5, 1] := 0.1498828607802206;
            c2[5, 2] := 0.5766417034027680;
            c2[6, 1] := 0.1367764717792823;
            c2[6, 2] := 0.4563528264410489;
            c2[7, 1] := 0.1209810465419295;
            c2[7, 2] := 0.3193782657322374;
            c2[8, 1] := 0.1016312648007554;
            c2[8, 2] := 0.1652419227369036;
          elseif order == 17 then
            alpha := 0.2106355148193306;
            c1[1] := 0.4093223608497299;
            c2[1, 1] := 0.1664014345826274;
            c2[1, 2] := 0.8067173752345952;
            c2[2, 1] := 0.1629839591538256;
            c2[2, 2] := 0.7712924931447541;
            c2[3, 1] := 0.1573277802512491;
            c2[3, 2] := 0.7134213666303411;
            c2[4, 1] := 0.1494828185148637;
            c2[4, 2] := 0.6347841731714884;
            c2[5, 1] := 0.1394948812681826;
            c2[5, 2] := 0.5375594414619047;
            c2[6, 1] := 0.1273627583380806;
            c2[6, 2] := 0.4241608926375478;
            c2[7, 1] := 0.1129187258461290;
            c2[7, 2] := 0.2965752009703245;
            c2[8, 1] := 0.9533357359908857e-1;
            c2[8, 2] := 0.1537041700889585;
          elseif order == 18 then
            alpha := 0.2044575288651841;
            c2[1, 1] := 0.1588768571976356;
            c2[1, 2] := 0.7951914263212913;
            c2[2, 1] := 0.1569357024981854;
            c2[2, 2] := 0.7744529690772538;
            c2[3, 1] := 0.1530722206358810;
            c2[3, 2] := 0.7335304425992080;
            c2[4, 1] := 0.1473206710524167;
            c2[4, 2] := 0.6735038935387268;
            c2[5, 1] := 0.1397225420331520;
            c2[5, 2] := 0.5959151542621590;
            c2[6, 1] := 0.1303092459809849;
            c2[6, 2] := 0.5026483447894845;
            c2[7, 1] := 0.1190627367060072;
            c2[7, 2] := 0.3956893824587150;
            c2[8, 1] := 0.1058058030798994;
            c2[8, 2] := 0.2765091830730650;
            c2[9, 1] := 0.8974708108800873e-1;
            c2[9, 2] := 0.1435505288284833;
          elseif order == 19 then
            alpha := 0.1987936248083529;
            c1[1] := 0.3892259966869526;
            c2[1, 1] := 0.1506640012172225;
            c2[1, 2] := 0.7693121733774260;
            c2[2, 1] := 0.1481728062796673;
            c2[2, 2] := 0.7421133586741549;
            c2[3, 1] := 0.1440444668388838;
            c2[3, 2] := 0.6975075386214800;
            c2[4, 1] := 0.1383101628540374;
            c2[4, 2] := 0.6365464378910025;
            c2[5, 1] := 0.1310032283190998;
            c2[5, 2] := 0.5606211948462122;
            c2[6, 1] := 0.1221431166405330;
            c2[6, 2] := 0.4713530424221445;
            c2[7, 1] := 0.1116991161103884;
            c2[7, 2] := 0.3703717538617073;
            c2[8, 1] := 0.9948917351196349e-1;
            c2[8, 2] := 0.2587371155559744;
            c2[9, 1] := 0.8475989238107367e-1;
            c2[9, 2] := 0.1345537894555993;
          elseif order == 20 then
            alpha := 0.1935761760416219;
            c2[1, 1] := 0.1443871348337404;
            c2[1, 2] := 0.7584165598446141;
            c2[2, 1] := 0.1429501891353184;
            c2[2, 2] := 0.7423000962318863;
            c2[3, 1] := 0.1400877384920004;
            c2[3, 2] := 0.7104185332215555;
            c2[4, 1] := 0.1358210369491446;
            c2[4, 2] := 0.6634599783272630;
            c2[5, 1] := 0.1301773703034290;
            c2[5, 2] := 0.6024175491895959;
            c2[6, 1] := 0.1231826501439148;
            c2[6, 2] := 0.5285332736326852;
            c2[7, 1] := 0.1148465498575254;
            c2[7, 2] := 0.4431977385498628;
            c2[8, 1] := 0.1051289462376788;
            c2[8, 2] := 0.3477444062821162;
            c2[9, 1] := 0.9384622797485121e-1;
            c2[9, 2] := 0.2429038300327729;
            c2[10, 1] := 0.8028211612831444e-1;
            c2[10, 2] := 0.1265329974009533;
          elseif order == 21 then
            alpha := 0.1887494014766075;
            c1[1] := 0.3718070668941645;
            c2[1, 1] := 0.1376151928386445;
            c2[1, 2] := 0.7364290859445481;
            c2[2, 1] := 0.1357438914390695;
            c2[2, 2] := 0.7150167318935022;
            c2[3, 1] := 0.1326398453462415;
            c2[3, 2] := 0.6798001808470175;
            c2[4, 1] := 0.1283231214897678;
            c2[4, 2] := 0.6314663440439816;
            c2[5, 1] := 0.1228169159777534;
            c2[5, 2] := 0.5709353626166905;
            c2[6, 1] := 0.1161406100773184;
            c2[6, 2] := 0.4993087153571335;
            c2[7, 1] := 0.1082959649233524;
            c2[7, 2] := 0.4177766148584385;
            c2[8, 1] := 0.9923596957485723e-1;
            c2[8, 2] := 0.3274257287232124;
            c2[9, 1] := 0.8877776108724853e-1;
            c2[9, 2] := 0.2287218166767916;
            c2[10, 1] := 0.7624076527736326e-1;
            c2[10, 2] := 0.1193423971506988;
          elseif order == 22 then
            alpha := 0.1842668221199706;
            c2[1, 1] := 0.1323053462701543;
            c2[1, 2] := 0.7262446126765204;
            c2[2, 1] := 0.1312121721769772;
            c2[2, 2] := 0.7134286088450949;
            c2[3, 1] := 0.1290330911166814;
            c2[3, 2] := 0.6880287870435514;
            c2[4, 1] := 0.1257817990372067;
            c2[4, 2] := 0.6505015800059301;
            c2[5, 1] := 0.1214765261983008;
            c2[5, 2] := 0.6015107185211451;
            c2[6, 1] := 0.1161365140967959;
            c2[6, 2] := 0.5418983553698413;
            c2[7, 1] := 0.1097755171533100;
            c2[7, 2] := 0.4726370779831614;
            c2[8, 1] := 0.1023889478519956;
            c2[8, 2] := 0.3947439506537486;
            c2[9, 1] := 0.9392485861253800e-1;
            c2[9, 2] := 0.3090996703083202;
            c2[10, 1] := 0.8420273775456455e-1;
            c2[10, 2] := 0.2159561978556017;
            c2[11, 1] := 0.7257600023938262e-1;
            c2[11, 2] := 0.1128633732721116;
          elseif order == 23 then
            alpha := 0.1800893554453722;
            c1[1] := 0.3565232673929280;
            c2[1, 1] := 0.1266275171652706;
            c2[1, 2] := 0.7072778066734162;
            c2[2, 1] := 0.1251865227648538;
            c2[2, 2] := 0.6900676345785905;
            c2[3, 1] := 0.1227944815236645;
            c2[3, 2] := 0.6617011100576023;
            c2[4, 1] := 0.1194647013077667;
            c2[4, 2] := 0.6226432315773119;
            c2[5, 1] := 0.1152132989252356;
            c2[5, 2] := 0.5735222810625359;
            c2[6, 1] := 0.1100558598478487;
            c2[6, 2] := 0.5151027978024605;
            c2[7, 1] := 0.1040013558214886;
            c2[7, 2] := 0.4482410942032739;
            c2[8, 1] := 0.9704014176512626e-1;
            c2[8, 2] := 0.3738049984631116;
            c2[9, 1] := 0.8911683905758054e-1;
            c2[9, 2] := 0.2925028692588410;
            c2[10, 1] := 0.8005438265072295e-1;
            c2[10, 2] := 0.2044134600278901;
            c2[11, 1] := 0.6923832296800832e-1;
            c2[11, 2] := 0.1069984887283394;
          elseif order == 24 then
            alpha := 0.1761838665838427;
            c2[1, 1] := 0.1220804912720132;
            c2[1, 2] := 0.6978026874156063;
            c2[2, 1] := 0.1212296762358897;
            c2[2, 2] := 0.6874139794926736;
            c2[3, 1] := 0.1195328372961027;
            c2[3, 2] := 0.6667954259551859;
            c2[4, 1] := 0.1169990987333593;
            c2[4, 2] := 0.6362602049901176;
            c2[5, 1] := 0.1136409040480130;
            c2[5, 2] := 0.5962662188435553;
            c2[6, 1] := 0.1094722001757955;
            c2[6, 2] := 0.5474001634109253;
            c2[7, 1] := 0.1045052832229087;
            c2[7, 2] := 0.4903523180249535;
            c2[8, 1] := 0.9874509806025907e-1;
            c2[8, 2] := 0.4258751523524645;
            c2[9, 1] := 0.9217799943472177e-1;
            c2[9, 2] := 0.3547079765396403;
            c2[10, 1] := 0.8474633796250476e-1;
            c2[10, 2] := 0.2774145482392767;
            c2[11, 1] := 0.7627722381240495e-1;
            c2[11, 2] := 0.1939329108084139;
            c2[12, 1] := 0.6618645465422745e-1;
            c2[12, 2] := 0.1016670147947242;
          elseif order == 25 then
            alpha := 0.1725220521949266;
            c1[1] := 0.3429735385896000;
            c2[1, 1] := 0.1172525033170618;
            c2[1, 2] := 0.6812327932576614;
            c2[2, 1] := 0.1161194585333535;
            c2[2, 2] := 0.6671566071153211;
            c2[3, 1] := 0.1142375145794466;
            c2[3, 2] := 0.6439167855053158;
            c2[4, 1] := 0.1116157454252308;
            c2[4, 2] := 0.6118378416180135;
            c2[5, 1] := 0.1082654809459177;
            c2[5, 2] := 0.5713609763370088;
            c2[6, 1] := 0.1041985674230918;
            c2[6, 2] := 0.5230289949762722;
            c2[7, 1] := 0.9942439308123559e-1;
            c2[7, 2] := 0.4674627926041906;
            c2[8, 1] := 0.9394453593830893e-1;
            c2[8, 2] := 0.4053226688298811;
            c2[9, 1] := 0.8774221237222533e-1;
            c2[9, 2] := 0.3372372276379071;
            c2[10, 1] := 0.8075839512216483e-1;
            c2[10, 2] := 0.2636485508005428;
            c2[11, 1] := 0.7282483286646764e-1;
            c2[11, 2] := 0.1843801345273085;
            c2[12, 1] := 0.6338571166846652e-1;
            c2[12, 2] := 0.9680153764737715e-1;
          elseif order == 26 then
            alpha := 0.1690795702796737;
            c2[1, 1] := 0.1133168695796030;
            c2[1, 2] := 0.6724297955493932;
            c2[2, 1] := 0.1126417845769961;
            c2[2, 2] := 0.6638709519790540;
            c2[3, 1] := 0.1112948749545606;
            c2[3, 2] := 0.6468652038763624;
            c2[4, 1] := 0.1092823986944244;
            c2[4, 2] := 0.6216337070799265;
            c2[5, 1] := 0.1066130386697976;
            c2[5, 2] := 0.5885011413992190;
            c2[6, 1] := 0.1032969057045413;
            c2[6, 2] := 0.5478864278297548;
            c2[7, 1] := 0.9934388184210715e-1;
            c2[7, 2] := 0.5002885306054287;
            c2[8, 1] := 0.9476081523436283e-1;
            c2[8, 2] := 0.4462644847551711;
            c2[9, 1] := 0.8954648464575577e-1;
            c2[9, 2] := 0.3863930785049522;
            c2[10, 1] := 0.8368166847159917e-1;
            c2[10, 2] := 0.3212074592527143;
            c2[11, 1] := 0.7710664731701103e-1;
            c2[11, 2] := 0.2510470347119383;
            c2[12, 1] := 0.6965807988411425e-1;
            c2[12, 2] := 0.1756419294111342;
            c2[13, 1] := 0.6080674930548766e-1;
            c2[13, 2] := 0.9234535279274277e-1;
          elseif order == 27 then
            alpha := 0.1658353543067995;
            c1[1] := 0.3308543720638957;
            c2[1, 1] := 0.1091618578712746;
            c2[1, 2] := 0.6577977071169651;
            c2[2, 1] := 0.1082549561495043;
            c2[2, 2] := 0.6461121666520275;
            c2[3, 1] := 0.1067479247890451;
            c2[3, 2] := 0.6267937760991321;
            c2[4, 1] := 0.1046471079537577;
            c2[4, 2] := 0.6000750116745808;
            c2[5, 1] := 0.1019605976654259;
            c2[5, 2] := 0.5662734183049320;
            c2[6, 1] := 0.9869726954433709e-1;
            c2[6, 2] := 0.5257827234948534;
            c2[7, 1] := 0.9486520934132483e-1;
            c2[7, 2] := 0.4790595019077763;
            c2[8, 1] := 0.9046906518775348e-1;
            c2[8, 2] := 0.4266025862147336;
            c2[9, 1] := 0.8550529998276152e-1;
            c2[9, 2] := 0.3689188223512328;
            c2[10, 1] := 0.7995282239306020e-1;
            c2[10, 2] := 0.3064589322702932;
            c2[11, 1] := 0.7375174596252882e-1;
            c2[11, 2] := 0.2394754504667310;
            c2[12, 1] := 0.6674377263329041e-1;
            c2[12, 2] := 0.1676223546666024;
            c2[13, 1] := 0.5842458027529246e-1;
            c2[13, 2] := 0.8825044329219431e-1;
          elseif order == 28 then
            alpha := 0.1627710671942929;
            c2[1, 1] := 0.1057232656113488;
            c2[1, 2] := 0.6496161226860832;
            c2[2, 1] := 0.1051786825724864;
            c2[2, 2] := 0.6424661279909941;
            c2[3, 1] := 0.1040917964935006;
            c2[3, 2] := 0.6282470268918791;
            c2[4, 1] := 0.1024670101953951;
            c2[4, 2] := 0.6071189030701136;
            c2[5, 1] := 0.1003105109519892;
            c2[5, 2] := 0.5793175191747016;
            c2[6, 1] := 0.9762969425430802e-1;
            c2[6, 2] := 0.5451486608855443;
            c2[7, 1] := 0.9443223803058400e-1;
            c2[7, 2] := 0.5049796971628137;
            c2[8, 1] := 0.9072460982036488e-1;
            c2[8, 2] := 0.4592270546572523;
            c2[9, 1] := 0.8650956423253280e-1;
            c2[9, 2] := 0.4083368605952977;
            c2[10, 1] := 0.8178165740374893e-1;
            c2[10, 2] := 0.3527525188880655;
            c2[11, 1] := 0.7651838885868020e-1;
            c2[11, 2] := 0.2928534570013572;
            c2[12, 1] := 0.7066010532447490e-1;
            c2[12, 2] := 0.2288185204390681;
            c2[13, 1] := 0.6405358596145789e-1;
            c2[13, 2] := 0.1602396172588190;
            c2[14, 1] := 0.5621780070227172e-1;
            c2[14, 2] := 0.8447589564915071e-1;
          elseif order == 29 then
            alpha := 0.1598706626277596;
            c1[1] := 0.3199314513011623;
            c2[1, 1] := 0.1021101032532951;
            c2[1, 2] := 0.6365758882240111;
            c2[2, 1] := 0.1013729819392774;
            c2[2, 2] := 0.6267495975736321;
            c2[3, 1] := 0.1001476175660628;
            c2[3, 2] := 0.6104876178266819;
            c2[4, 1] := 0.9843854640428316e-1;
            c2[4, 2] := 0.5879603139195113;
            c2[5, 1] := 0.9625164534591696e-1;
            c2[5, 2] := 0.5594012291050210;
            c2[6, 1] := 0.9359356960417668e-1;
            c2[6, 2] := 0.5251016150410664;
            c2[7, 1] := 0.9047086748649986e-1;
            c2[7, 2] := 0.4854024475590397;
            c2[8, 1] := 0.8688856407189167e-1;
            c2[8, 2] := 0.4406826457109709;
            c2[9, 1] := 0.8284779224069856e-1;
            c2[9, 2] := 0.3913408089298914;
            c2[10, 1] := 0.7834154620997181e-1;
            c2[10, 2] := 0.3377643999400627;
            c2[11, 1] := 0.7334628941928766e-1;
            c2[11, 2] := 0.2802710651919946;
            c2[12, 1] := 0.6780290487362146e-1;
            c2[12, 2] := 0.2189770008083379;
            c2[13, 1] := 0.6156321231528423e-1;
            c2[13, 2] := 0.1534235999306070;
            c2[14, 1] := 0.5416797446761512e-1;
            c2[14, 2] := 0.8098664736760292e-1;
          elseif order == 30 then
            alpha := 0.1571200296252450;
            c2[1, 1] := 0.9908074847842124e-1;
            c2[1, 2] := 0.6289618807831557;
            c2[2, 1] := 0.9863509708328196e-1;
            c2[2, 2] := 0.6229164525571278;
            c2[3, 1] := 0.9774542692037148e-1;
            c2[3, 2] := 0.6108853364240036;
            c2[4, 1] := 0.9641490581986484e-1;
            c2[4, 2] := 0.5929869253412513;
            c2[5, 1] := 0.9464802912225441e-1;
            c2[5, 2] := 0.5693960175547550;
            c2[6, 1] := 0.9245027206218041e-1;
            c2[6, 2] := 0.5403402396359503;
            c2[7, 1] := 0.8982754584112941e-1;
            c2[7, 2] := 0.5060948065875106;
            c2[8, 1] := 0.8678535291732599e-1;
            c2[8, 2] := 0.4669749797983789;
            c2[9, 1] := 0.8332744242052199e-1;
            c2[9, 2] := 0.4233249626334694;
            c2[10, 1] := 0.7945356393775309e-1;
            c2[10, 2] := 0.3755006094498054;
            c2[11, 1] := 0.7515543969833788e-1;
            c2[11, 2] := 0.3238400339292700;
            c2[12, 1] := 0.7040879901685638e-1;
            c2[12, 2] := 0.2686072427439079;
            c2[13, 1] := 0.6515528854010540e-1;
            c2[13, 2] := 0.2098650589782619;
            c2[14, 1] := 0.5925168237177876e-1;
            c2[14, 2] := 0.1471138832654873;
            c2[15, 1] := 0.5225913954211672e-1;
            c2[15, 2] := 0.7775248839507864e-1;
          elseif order == 31 then
            alpha := 0.1545067022920929;
            c1[1] := 0.3100206996451866;
            c2[1, 1] := 0.9591020358831668e-1;
            c2[1, 2] := 0.6172474793293396;
            c2[2, 1] := 0.9530301275601203e-1;
            c2[2, 2] := 0.6088916323460413;
            c2[3, 1] := 0.9429332655402368e-1;
            c2[3, 2] := 0.5950511595503025;
            c2[4, 1] := 0.9288445429894548e-1;
            c2[4, 2] := 0.5758534119053522;
            c2[5, 1] := 0.9108073420087422e-1;
            c2[5, 2] := 0.5514734636081183;
            c2[6, 1] := 0.8888719137536870e-1;
            c2[6, 2] := 0.5221306199481831;
            c2[7, 1] := 0.8630901440239650e-1;
            c2[7, 2] := 0.4880834248148061;
            c2[8, 1] := 0.8335074993373294e-1;
            c2[8, 2] := 0.4496225358496770;
            c2[9, 1] := 0.8001502494376102e-1;
            c2[9, 2] := 0.4070602306679052;
            c2[10, 1] := 0.7630041338037624e-1;
            c2[10, 2] := 0.3607139804818122;
            c2[11, 1] := 0.7219760885744920e-1;
            c2[11, 2] := 0.3108783301229550;
            c2[12, 1] := 0.6768185077153345e-1;
            c2[12, 2] := 0.2577706252514497;
            c2[13, 1] := 0.6269571766328638e-1;
            c2[13, 2] := 0.2014081375889921;
            c2[14, 1] := 0.5710081766945065e-1;
            c2[14, 2] := 0.1412581515841926;
            c2[15, 1] := 0.5047740914807019e-1;
            c2[15, 2] := 0.7474725873250158e-1;
          elseif order == 32 then
            alpha := 0.1520196210848210;
            c2[1, 1] := 0.9322163554339406e-1;
            c2[1, 2] := 0.6101488690506050;
            c2[2, 1] := 0.9285233997694042e-1;
            c2[2, 2] := 0.6049832320721264;
            c2[3, 1] := 0.9211494244473163e-1;
            c2[3, 2] := 0.5946969295569034;
            c2[4, 1] := 0.9101176786042449e-1;
            c2[4, 2] := 0.5793791854364477;
            c2[5, 1] := 0.8954614071360517e-1;
            c2[5, 2] := 0.5591619969234026;
            c2[6, 1] := 0.8772216763680164e-1;
            c2[6, 2] := 0.5342177994699602;
            c2[7, 1] := 0.8554440426912734e-1;
            c2[7, 2] := 0.5047560942986598;
            c2[8, 1] := 0.8301735302045588e-1;
            c2[8, 2] := 0.4710187048140929;
            c2[9, 1] := 0.8014469519188161e-1;
            c2[9, 2] := 0.4332730387207936;
            c2[10, 1] := 0.7692807528893225e-1;
            c2[10, 2] := 0.3918021436411035;
            c2[11, 1] := 0.7336507157284898e-1;
            c2[11, 2] := 0.3468890521471250;
            c2[12, 1] := 0.6944555312763458e-1;
            c2[12, 2] := 0.2987898029050460;
            c2[13, 1] := 0.6514446669420571e-1;
            c2[13, 2] := 0.2476810747407199;
            c2[14, 1] := 0.6040544477732702e-1;
            c2[14, 2] := 0.1935412053397663;
            c2[15, 1] := 0.5509478650672775e-1;
            c2[15, 2] := 0.1358108994174911;
            c2[16, 1] := 0.4881064725720192e-1;
            c2[16, 2] := 0.7194819894416505e-1;
          elseif order == 33 then
            alpha := 0.1496489351138032;
            c1[1] := 0.3009752799176432;
            c2[1, 1] := 0.9041725460994505e-1;
            c2[1, 2] := 0.5995521047364046;
            c2[2, 1] := 0.8991117804113002e-1;
            c2[2, 2] := 0.5923764112099496;
            c2[3, 1] := 0.8906941547422532e-1;
            c2[3, 2] := 0.5804822013853129;
            c2[4, 1] := 0.8789442491445575e-1;
            c2[4, 2] := 0.5639663528946501;
            c2[5, 1] := 0.8638945831033775e-1;
            c2[5, 2] := 0.5429623519607796;
            c2[6, 1] := 0.8455834602616358e-1;
            c2[6, 2] := 0.5176379938389326;
            c2[7, 1] := 0.8240517431382334e-1;
            c2[7, 2] := 0.4881921474066189;
            c2[8, 1] := 0.7993380417355076e-1;
            c2[8, 2] := 0.4548502528082586;
            c2[9, 1] := 0.7714713890732801e-1;
            c2[9, 2] := 0.4178579388038483;
            c2[10, 1] := 0.7404596598181127e-1;
            c2[10, 2] := 0.3774715722484659;
            c2[11, 1] := 0.7062702339160462e-1;
            c2[11, 2] := 0.3339432938810453;
            c2[12, 1] := 0.6687952672391507e-1;
            c2[12, 2] := 0.2874950693388235;
            c2[13, 1] := 0.6277828912909767e-1;
            c2[13, 2] := 0.2382680702894708;
            c2[14, 1] := 0.5826808305383988e-1;
            c2[14, 2] := 0.1862073169968455;
            c2[15, 1] := 0.5321974125363517e-1;
            c2[15, 2] := 0.1307323751236313;
            c2[16, 1] := 0.4724820282032780e-1;
            c2[16, 2] := 0.6933542082177094e-1;
          elseif order == 34 then
            alpha := 0.1473858373968463;
            c2[1, 1] := 0.8801537152275983e-1;
            c2[1, 2] := 0.5929204288972172;
            c2[2, 1] := 0.8770594341007476e-1;
            c2[2, 2] := 0.5884653382247518;
            c2[3, 1] := 0.8708797598072095e-1;
            c2[3, 2] := 0.5795895850253119;
            c2[4, 1] := 0.8616320590689187e-1;
            c2[4, 2] := 0.5663615383647170;
            c2[5, 1] := 0.8493413175570858e-1;
            c2[5, 2] := 0.5488825092350877;
            c2[6, 1] := 0.8340387368687513e-1;
            c2[6, 2] := 0.5272851839324592;
            c2[7, 1] := 0.8157596213131521e-1;
            c2[7, 2] := 0.5017313864372913;
            c2[8, 1] := 0.7945402670834270e-1;
            c2[8, 2] := 0.4724089864574216;
            c2[9, 1] := 0.7704133559556429e-1;
            c2[9, 2] := 0.4395276256463053;
            c2[10, 1] := 0.7434009635219704e-1;
            c2[10, 2] := 0.4033126590648964;
            c2[11, 1] := 0.7135035113853376e-1;
            c2[11, 2] := 0.3639961488919042;
            c2[12, 1] := 0.6806813160738834e-1;
            c2[12, 2] := 0.3218025212900124;
            c2[13, 1] := 0.6448214312000864e-1;
            c2[13, 2] := 0.2769235521088158;
            c2[14, 1] := 0.6056719318430530e-1;
            c2[14, 2] := 0.2294693573271038;
            c2[15, 1] := 0.5626925196925040e-1;
            c2[15, 2] := 0.1793564218840015;
            c2[16, 1] := 0.5146352031547277e-1;
            c2[16, 2] := 0.1259877129326412;
            c2[17, 1] := 0.4578069074410591e-1;
            c2[17, 2] := 0.6689147319568768e-1;
          elseif order == 35 then
            alpha := 0.1452224267615486;
            c1[1] := 0.2926764667564367;
            c2[1, 1] := 0.8551731299267280e-1;
            c2[1, 2] := 0.5832758214629523;
            c2[2, 1] := 0.8509109732853060e-1;
            c2[2, 2] := 0.5770596582643844;
            c2[3, 1] := 0.8438201446671953e-1;
            c2[3, 2] := 0.5667497616665494;
            c2[4, 1] := 0.8339191981579831e-1;
            c2[4, 2] := 0.5524209816238369;
            c2[5, 1] := 0.8212328610083385e-1;
            c2[5, 2] := 0.5341766459916322;
            c2[6, 1] := 0.8057906332198853e-1;
            c2[6, 2] := 0.5121470053512750;
            c2[7, 1] := 0.7876247299954955e-1;
            c2[7, 2] := 0.4864870722254752;
            c2[8, 1] := 0.7667670879950268e-1;
            c2[8, 2] := 0.4573736721705665;
            c2[9, 1] := 0.7432449556218945e-1;
            c2[9, 2] := 0.4250013835198991;
            c2[10, 1] := 0.7170742126011575e-1;
            c2[10, 2] := 0.3895767735915445;
            c2[11, 1] := 0.6882488171701314e-1;
            c2[11, 2] := 0.3513097926737368;
            c2[12, 1] := 0.6567231746957568e-1;
            c2[12, 2] := 0.3103999917596611;
            c2[13, 1] := 0.6223804362223595e-1;
            c2[13, 2] := 0.2670123611280899;
            c2[14, 1] := 0.5849696460782910e-1;
            c2[14, 2] := 0.2212298104867592;
            c2[15, 1] := 0.5439628409499822e-1;
            c2[15, 2] := 0.1729443731341637;
            c2[16, 1] := 0.4981540179136920e-1;
            c2[16, 2] := 0.1215462157134930;
            c2[17, 1] := 0.4439981033536435e-1;
            c2[17, 2] := 0.6460098363520967e-1;
          elseif order == 36 then
            alpha := 0.1431515914458580;
            c2[1, 1] := 0.8335881847130301e-1;
            c2[1, 2] := 0.5770670512160201;
            c2[2, 1] := 0.8309698922852212e-1;
            c2[2, 2] := 0.5731929100172432;
            c2[3, 1] := 0.8257400347039723e-1;
            c2[3, 2] := 0.5654713811993058;
            c2[4, 1] := 0.8179117911600136e-1;
            c2[4, 2] := 0.5539556343603020;
            c2[5, 1] := 0.8075042173126963e-1;
            c2[5, 2] := 0.5387245649546684;
            c2[6, 1] := 0.7945413151258206e-1;
            c2[6, 2] := 0.5198817177723069;
            c2[7, 1] := 0.7790506514288866e-1;
            c2[7, 2] := 0.4975537629595409;
            c2[8, 1] := 0.7610613635339480e-1;
            c2[8, 2] := 0.4718884193866789;
            c2[9, 1] := 0.7406012816626425e-1;
            c2[9, 2] := 0.4430516443136726;
            c2[10, 1] := 0.7176927060205631e-1;
            c2[10, 2] := 0.4112237708115829;
            c2[11, 1] := 0.6923460172504251e-1;
            c2[11, 2] := 0.3765940116389730;
            c2[12, 1] := 0.6645495833489556e-1;
            c2[12, 2] := 0.3393522147815403;
            c2[13, 1] := 0.6342528888937094e-1;
            c2[13, 2] := 0.2996755899575573;
            c2[14, 1] := 0.6013361864949449e-1;
            c2[14, 2] := 0.2577053294053830;
            c2[15, 1] := 0.5655503081322404e-1;
            c2[15, 2] := 0.2135004731531631;
            c2[16, 1] := 0.5263798119559069e-1;
            c2[16, 2] := 0.1669320999865636;
            c2[17, 1] := 0.4826589873626196e-1;
            c2[17, 2] := 0.1173807590715484;
            c2[18, 1] := 0.4309819397289806e-1;
            c2[18, 2] := 0.6245036108880222e-1;
          elseif order == 37 then
            alpha := 0.1411669104782917;
            c1[1] := 0.2850271036215707;
            c2[1, 1] := 0.8111958235023328e-1;
            c2[1, 2] := 0.5682412610563970;
            c2[2, 1] := 0.8075727567979578e-1;
            c2[2, 2] := 0.5628142923227016;
            c2[3, 1] := 0.8015440554413301e-1;
            c2[3, 2] := 0.5538087696879930;
            c2[4, 1] := 0.7931239302677386e-1;
            c2[4, 2] := 0.5412833323304460;
            c2[5, 1] := 0.7823314328639347e-1;
            c2[5, 2] := 0.5253190555393968;
            c2[6, 1] := 0.7691895211595101e-1;
            c2[6, 2] := 0.5060183741977191;
            c2[7, 1] := 0.7537237072011853e-1;
            c2[7, 2] := 0.4835036020049034;
            c2[8, 1] := 0.7359601294804538e-1;
            c2[8, 2] := 0.4579149413954837;
            c2[9, 1] := 0.7159227884849299e-1;
            c2[9, 2] := 0.4294078049978829;
            c2[10, 1] := 0.6936295002846032e-1;
            c2[10, 2] := 0.3981491350382047;
            c2[11, 1] := 0.6690857785828917e-1;
            c2[11, 2] := 0.3643121502867948;
            c2[12, 1] := 0.6422751692085542e-1;
            c2[12, 2] := 0.3280684291406284;
            c2[13, 1] := 0.6131430866206096e-1;
            c2[13, 2] := 0.2895750997170303;
            c2[14, 1] := 0.5815677249570920e-1;
            c2[14, 2] := 0.2489521814805720;
            c2[15, 1] := 0.5473023527947980e-1;
            c2[15, 2] := 0.2062377435955363;
            c2[16, 1] := 0.5098441033167034e-1;
            c2[16, 2] := 0.1612849131645336;
            c2[17, 1] := 0.4680658811093562e-1;
            c2[17, 2] := 0.1134672937045305;
            c2[18, 1] := 0.4186928031694695e-1;
            c2[18, 2] := 0.6042754777339966e-1;
          elseif order == 38 then
            alpha := 0.1392625697140030;
            c2[1, 1] := 0.7916943373658329e-1;
            c2[1, 2] := 0.5624158631591745;
            c2[2, 1] := 0.7894592250257840e-1;
            c2[2, 2] := 0.5590219398777304;
            c2[3, 1] := 0.7849941672384930e-1;
            c2[3, 2] := 0.5522551628416841;
            c2[4, 1] := 0.7783093084875645e-1;
            c2[4, 2] := 0.5421574325808380;
            c2[5, 1] := 0.7694193770482690e-1;
            c2[5, 2] := 0.5287909941093643;
            c2[6, 1] := 0.7583430534712885e-1;
            c2[6, 2] := 0.5122376814029880;
            c2[7, 1] := 0.7451020436122948e-1;
            c2[7, 2] := 0.4925978555548549;
            c2[8, 1] := 0.7297197617673508e-1;
            c2[8, 2] := 0.4699889739625235;
            c2[9, 1] := 0.7122194706992953e-1;
            c2[9, 2] := 0.4445436860615774;
            c2[10, 1] := 0.6926216260386816e-1;
            c2[10, 2] := 0.4164072786327193;
            c2[11, 1] := 0.6709399961255503e-1;
            c2[11, 2] := 0.3857341621868851;
            c2[12, 1] := 0.6471757977022456e-1;
            c2[12, 2] := 0.3526828388476838;
            c2[13, 1] := 0.6213084287116965e-1;
            c2[13, 2] := 0.3174082831364342;
            c2[14, 1] := 0.5932799638550641e-1;
            c2[14, 2] := 0.2800495563550299;
            c2[15, 1] := 0.5629672408524944e-1;
            c2[15, 2] := 0.2407078154782509;
            c2[16, 1] := 0.5301264751544952e-1;
            c2[16, 2] := 0.1994026830553859;
            c2[17, 1] := 0.4942673259817896e-1;
            c2[17, 2] := 0.1559719194038917;
            c2[18, 1] := 0.4542996716979947e-1;
            c2[18, 2] := 0.1097844277878470;
            c2[19, 1] := 0.4070720755433961e-1;
            c2[19, 2] := 0.5852181110523043e-1;
          elseif order == 39 then
            alpha := 0.1374332900196804;
            c1[1] := 0.2779468246419593;
            c2[1, 1] := 0.7715084161825772e-1;
            c2[1, 2] := 0.5543001331300056;
            c2[2, 1] := 0.7684028301163326e-1;
            c2[2, 2] := 0.5495289890712267;
            c2[3, 1] := 0.7632343924866024e-1;
            c2[3, 2] := 0.5416083298429741;
            c2[4, 1] := 0.7560141319808483e-1;
            c2[4, 2] := 0.5305846713929198;
            c2[5, 1] := 0.7467569064745969e-1;
            c2[5, 2] := 0.5165224112570647;
            c2[6, 1] := 0.7354807648551346e-1;
            c2[6, 2] := 0.4995030679271456;
            c2[7, 1] := 0.7222060351121389e-1;
            c2[7, 2] := 0.4796242430956156;
            c2[8, 1] := 0.7069540462458585e-1;
            c2[8, 2] := 0.4569982440368368;
            c2[9, 1] := 0.6897453353492381e-1;
            c2[9, 2] := 0.4317502624832354;
            c2[10, 1] := 0.6705970959388781e-1;
            c2[10, 2] := 0.4040159353969854;
            c2[11, 1] := 0.6495194541066725e-1;
            c2[11, 2] := 0.3739379843169939;
            c2[12, 1] := 0.6265098412417610e-1;
            c2[12, 2] := 0.3416613843816217;
            c2[13, 1] := 0.6015440984955930e-1;
            c2[13, 2] := 0.3073260166338746;
            c2[14, 1] := 0.5745615876877304e-1;
            c2[14, 2] := 0.2710546723961181;
            c2[15, 1] := 0.5454383762391338e-1;
            c2[15, 2] := 0.2329316824061170;
            c2[16, 1] := 0.5139340231935751e-1;
            c2[16, 2] := 0.1929604256043231;
            c2[17, 1] := 0.4795705862458131e-1;
            c2[17, 2] := 0.1509655259246037;
            c2[18, 1] := 0.4412933231935506e-1;
            c2[18, 2] := 0.1063130748962878;
            c2[19, 1] := 0.3960672309405603e-1;
            c2[19, 2] := 0.5672356837211527e-1;
          elseif order == 40 then
            alpha := 0.1356742655825434;
            c2[1, 1] := 0.7538038374294594e-1;
            c2[1, 2] := 0.5488228264329617;
            c2[2, 1] := 0.7518806529402738e-1;
            c2[2, 2] := 0.5458297722483311;
            c2[3, 1] := 0.7480383050347119e-1;
            c2[3, 2] := 0.5398604576730540;
            c2[4, 1] := 0.7422847031965465e-1;
            c2[4, 2] := 0.5309482987446206;
            c2[5, 1] := 0.7346313704205006e-1;
            c2[5, 2] := 0.5191429845322307;
            c2[6, 1] := 0.7250930053201402e-1;
            c2[6, 2] := 0.5045099368431007;
            c2[7, 1] := 0.7136868456879621e-1;
            c2[7, 2] := 0.4871295553902607;
            c2[8, 1] := 0.7004317764946634e-1;
            c2[8, 2] := 0.4670962098860498;
            c2[9, 1] := 0.6853470921527828e-1;
            c2[9, 2] := 0.4445169164956202;
            c2[10, 1] := 0.6684507689945471e-1;
            c2[10, 2] := 0.4195095960479698;
            c2[11, 1] := 0.6497570123412630e-1;
            c2[11, 2] := 0.3922007419030645;
            c2[12, 1] := 0.6292726794917847e-1;
            c2[12, 2] := 0.3627221993494397;
            c2[13, 1] := 0.6069918741663154e-1;
            c2[13, 2] := 0.3312065181294388;
            c2[14, 1] := 0.5828873983769410e-1;
            c2[14, 2] := 0.2977798532686911;
            c2[15, 1] := 0.5568964389813015e-1;
            c2[15, 2] := 0.2625503293999835;
            c2[16, 1] := 0.5288947816690705e-1;
            c2[16, 2] := 0.2255872486520188;
            c2[17, 1] := 0.4986456327645859e-1;
            c2[17, 2] := 0.1868796731919594;
            c2[18, 1] := 0.4656832613054458e-1;
            c2[18, 2] := 0.1462410193532463;
            c2[19, 1] := 0.4289867647614935e-1;
            c2[19, 2] := 0.1030361558710747;
            c2[20, 1] := 0.3856310684054106e-1;
            c2[20, 2] := 0.5502423832293889e-1;
          elseif order == 41 then
            alpha := 0.1339811106984253;
            c1[1] := 0.2713685065531391;
            c2[1, 1] := 0.7355140275160984e-1;
            c2[1, 2] := 0.5413274778282860;
            c2[2, 1] := 0.7328319082267173e-1;
            c2[2, 2] := 0.5371064088294270;
            c2[3, 1] := 0.7283676160772547e-1;
            c2[3, 2] := 0.5300963437270770;
            c2[4, 1] := 0.7221298133014343e-1;
            c2[4, 2] := 0.5203345998371490;
            c2[5, 1] := 0.7141302173623395e-1;
            c2[5, 2] := 0.5078728971879841;
            c2[6, 1] := 0.7043831559982149e-1;
            c2[6, 2] := 0.4927768111819803;
            c2[7, 1] := 0.6929049381827268e-1;
            c2[7, 2] := 0.4751250308594139;
            c2[8, 1] := 0.6797129849758392e-1;
            c2[8, 2] := 0.4550083840638406;
            c2[9, 1] := 0.6648246325101609e-1;
            c2[9, 2] := 0.4325285673076087;
            c2[10, 1] := 0.6482554675958526e-1;
            c2[10, 2] := 0.4077964789091151;
            c2[11, 1] := 0.6300169683004558e-1;
            c2[11, 2] := 0.3809299858742483;
            c2[12, 1] := 0.6101130648543355e-1;
            c2[12, 2] := 0.3520508315700898;
            c2[13, 1] := 0.5885349417435808e-1;
            c2[13, 2] := 0.3212801560701271;
            c2[14, 1] := 0.5652528148656809e-1;
            c2[14, 2] := 0.2887316252774887;
            c2[15, 1] := 0.5402021575818373e-1;
            c2[15, 2] := 0.2545001287790888;
            c2[16, 1] := 0.5132588802608274e-1;
            c2[16, 2] := 0.2186415296842951;
            c2[17, 1] := 0.4841900639702602e-1;
            c2[17, 2] := 0.1811322622296060;
            c2[18, 1] := 0.4525419574485134e-1;
            c2[18, 2] := 0.1417762065404688;
            c2[19, 1] := 0.4173260173087802e-1;
            c2[19, 2] := 0.9993834530966510e-1;
            c2[20, 1] := 0.3757210572966463e-1;
            c2[20, 2] := 0.5341611499960143e-1;
          else
            Streams.error("Input argument order (= " + String(order) +
              ") of Bessel filter is not in the range 1..41");
          end if;

          annotation (Documentation(info="<html><p>The transfer function H(p) of a <i>n</i> 'th order Bessel filter is given by</p>
<blockquote><pre>
        Bn(0)
H(p) = -------
        Bn(p)
 </pre>
</blockquote>
<p>with the denominator polynomial</p>
<blockquote><pre>
         n             n  (2n - k)!       p^k
Bn(p) = sum c_k*p^k = sum ----------- * -------   (1)
        k=0           k=0 (n - k)!k!    2^(n-k)
</pre></blockquote>
<p>and the numerator</p>
<blockquote><pre>
               (2n)!     1
Bn(0) = c_0 = ------- * ---- .                    (2)
                n!      2^n
 </pre></blockquote>
<p>Although the coefficients c_k are integer numbers, it is not advisable to use the
polynomials in an unfactorized form because the coefficients are fast growing with order
n (c_0 is approximately 0.3e24 and 0.8e59 for order n=20 and order n=40
respectively).</p>

<p>Therefore, the polynomial Bn(p) is factorized to first and second order polynomials with
real coefficients corresponding to zeros and poles representation that is used in this library.</p>

<p>The function returns the coefficients which resulted from factorization of the normalized transfer function</p>
<blockquote><pre>
H'(p') = H(p),  p' = p/w0
</pre></blockquote>
<p>as well as</p>
<blockquote><pre>
alpha = 1/w0
</pre></blockquote>
<p>the reciprocal of the cut of frequency w0 where the gain of the transfer function is
decreased 3dB.</p>

<p>Both, coefficients and cut off frequency were calculated symbolically and were eventually evaluated
with high precision calculation. The results were stored in this function as real
numbers.</p>

<h4>Calculation of normalized Bessel filter coefficients</h4>
<p>Equation</p>
<blockquote><pre>
abs(H(j*w0)) = abs(Bn(0)/Bn(j*w0)) = 10^(-3/20)
</pre></blockquote>
<p>which must be fulfilled for cut off frequency w = w0 leads to</p>
<blockquote><pre>
[Re(Bn(j*w0))]^2 + [Im(Bn(j*w0))]^2 - (Bn(0)^2)*10^(3/10) = 0
</pre></blockquote>
<p>which has exactly one real solution w0 for each order n. This solutions of w0 are
calculated symbolically first and evaluated by using high precise values of the
coefficients c_k calculated by following (1) and (2).</p>

<p>With w0, the coefficients of the factorized polynomial can be computed by calculating the
zeros of the denominator polynomial</p>
<blockquote><pre>
        n
Bn(p) = sum w0^k*c_k*(p/w0)^k
        k=0
</pre></blockquote>
<p>of the normalized transfer function H'(p'). There exist n/2 of conjugate complex
pairs of zeros (beta +-j*gamma) if n is even and one additional real zero (alpha) if n is
odd. Finally, the coefficients a, b1_k, b2_k of the polynomials</p>
<blockquote><pre> a*p + 1,  n is odd </pre></blockquote>
<p>and</p>
<blockquote><pre>
b2_k*p^2 + b1_k*p + 1,   k = 1,... div(n,2)
</pre></blockquote>
<p>results from</p>
<blockquote><pre>
a = -1/alpha
</pre></blockquote>
<p>and</p>
<blockquote><pre>
b2_k = 1/(beta_k^2 + gamma_k^2) b1_k = -2*beta_k/(beta_k^2 + gamma_k^2)
</pre></blockquote>
</html>"));
        end BesselBaseCoefficients;

        function toHighestPowerOne
          "Transform filter to form with highest power of s equal 1"
          extends Modelica.Icons.Function;

          input Real den1[:] "[s] coefficients of polynomials (den1[i]*s + 1)";
          input Real den2[:,2]
            "[s^2, s] coefficients of polynomials (den2[i,1]*s^2 + den2[i,2]*s + 1)";
          output Real cr[size(den1, 1)]
            "[s^0] coefficients of polynomials cr[i]*(s+1/cr[i])";
          output Real c0[size(den2, 1)]
            "[s^0] coefficients of polynomials (s^2 + (den2[i,2]/den2[i,1])*s + (1/den2[i,1]))";
          output Real c1[size(den2, 1)]
            "[s^1] coefficients of polynomials (s^2 + (den2[i,2]/den2[i,1])*s + (1/den2[i,1]))";
        algorithm
          for i in 1:size(den1, 1) loop
            cr[i] := 1/den1[i];
          end for;

          for i in 1:size(den2, 1) loop
            c1[i] := den2[i, 2]/den2[i, 1];
            c0[i] := 1/den2[i, 1];
          end for;
        end toHighestPowerOne;

        function normalizationFactor
          "Compute correction factor of low pass filter such that amplitude at cut-off frequency is -3db (=10^(-3/20) = 0.70794...)"
          extends Modelica.Icons.Function;

          import Modelica;
          import Modelica.Utilities.Streams;

          input Real c1[:]
            "[p] coefficients of denominator polynomials (c1[i}*p + 1)";
          input Real c2[:,2]
            "[p^2, p] coefficients of denominator polynomials (c2[i,1]*p^2 + c2[i,2]*p + 1)";
          output Real alpha "Correction factor (replace p by alpha*p)";
        protected
          Real alpha_min;
          Real alpha_max;

          function normalizationResidue
            "Residue of correction factor computation"
            extends Modelica.Icons.Function;
            input Real c1[:]
              "[p] coefficients of denominator polynomials (c1[i]*p + 1)";
            input Real c2[:,2]
              "[p^2, p] coefficients of denominator polynomials (c2[i,1]*p^2 + c2[i,2]*p + 1)";
            input Real alpha;
            output Real residue;
          protected
            constant Real beta= 10^(-3/20)
              "Amplitude of -3db required, i.e., -3db = 20*log(beta)";
            Real cc1;
            Real cc2;
            Real p;
            Real alpha2=alpha*alpha;
            Real alpha4=alpha2*alpha2;
            Real A2=1.0;
          algorithm
            assert(size(c1,1) <= 1, "Internal error 2 (should not occur)");
            if size(c1, 1) == 1 then
              cc1 := c1[1]*c1[1];
              p := 1 + cc1*alpha2;
              A2 := A2*p;
            end if;
            for i in 1:size(c2, 1) loop
              cc1 := c2[i, 2]*c2[i, 2] - 2*c2[i, 1];
              cc2 := c2[i, 1]*c2[i, 1];
              p := 1 + cc1*alpha2 + cc2*alpha4;
              A2 := A2*p;
            end for;
            residue := 1/sqrt(A2) - beta;
          end normalizationResidue;

          function findInterval "Find interval for the root"
            extends Modelica.Icons.Function;
            input Real c1[:]
              "[p] coefficients of denominator polynomials (a*p + 1)";
            input Real c2[:,2]
              "[p^2, p] coefficients of denominator polynomials (b*p^2 + a*p + 1)";
            output Real alpha_min;
            output Real alpha_max;
          protected
            Real alpha = 1.0;
            Real residue;
          algorithm
            alpha_min :=0;
            residue := normalizationResidue(c1, c2, alpha);
            if residue < 0 then
               alpha_max :=alpha;
            else
               while residue >= 0 loop
                  alpha := 1.1*alpha;
                  residue := normalizationResidue(c1, c2, alpha);
               end while;
               alpha_max :=alpha;
            end if;
          end findInterval;

        function solveOneNonlinearEquation
            "Solve f(u) = 0; f(u_min) and f(u_max) must have different signs"
            extends Modelica.Icons.Function;
            import Modelica.Utilities.Streams.error;

          input Real c1[:]
              "[p] coefficients of denominator polynomials (c1[i]*p + 1)";
          input Real c2[:,2]
              "[p^2, p] coefficients of denominator polynomials (c2[i,1]*p^2 + c2[i,2]*p + 1)";
          input Real u_min "Lower bound of search interval";
          input Real u_max "Upper bound of search interval";
          input Real tolerance=100*Modelica.Constants.eps
              "Relative tolerance of solution u";
          output Real u "Value of independent variable so that f(u) = 0";

          protected
          constant Real eps=Modelica.Constants.eps "machine epsilon";
          Real a=u_min "Current best minimum interval value";
          Real b=u_max "Current best maximum interval value";
          Real c "Intermediate point a <= c <= b";
          Real d;
          Real e "b - a";
          Real m;
          Real s;
          Real p;
          Real q;
          Real r;
          Real tol;
          Real fa "= f(a)";
          Real fb "= f(b)";
          Real fc;
          Boolean found=false;
        algorithm
          // Check that f(u_min) and f(u_max) have different sign
          fa := normalizationResidue(c1,c2,u_min);
          fb := normalizationResidue(c1,c2,u_max);
          fc := fb;
          if fa > 0.0 and fb > 0.0 or fa < 0.0 and fb < 0.0 then
            error(
              "The arguments u_min and u_max to solveOneNonlinearEquation(..)\n" +
              "do not bracket the root of the single non-linear equation:\n" +
              "  u_min  = " + String(u_min) + "\n" + "  u_max  = " + String(u_max)
               + "\n" + "  fa = f(u_min) = " + String(fa) + "\n" +
              "  fb = f(u_max) = " + String(fb) + "\n" +
              "fa and fb must have opposite sign which is not the case");
          end if;

          // Initialize variables
          c := a;
          fc := fa;
          e := b - a;
          d := e;

          // Search loop
          while not found loop
            if abs(fc) < abs(fb) then
              a := b;
              b := c;
              c := a;
              fa := fb;
              fb := fc;
              fc := fa;
            end if;

            tol := 2*eps*abs(b) + tolerance;
            m := (c - b)/2;

            if abs(m) <= tol or fb == 0.0 then
              // root found (interval is small enough)
              found := true;
              u := b;
            else
              // Determine if a bisection is needed
              if abs(e) < tol or abs(fa) <= abs(fb) then
                e := m;
                d := e;
              else
                s := fb/fa;
                if a == c then
                  // linear interpolation
                  p := 2*m*s;
                  q := 1 - s;
                else
                  // inverse quadratic interpolation
                  q := fa/fc;
                  r := fb/fc;
                  p := s*(2*m*q*(q - r) - (b - a)*(r - 1));
                  q := (q - 1)*(r - 1)*(s - 1);
                end if;

                if p > 0 then
                  q := -q;
                else
                  p := -p;
                end if;

                s := e;
                e := d;
                if 2*p < 3*m*q - abs(tol*q) and p < abs(0.5*s*q) then
                  // interpolation successful
                  d := p/q;
                else
                  // use bi-section
                  e := m;
                  d := e;
                end if;
              end if;

              // Best guess value is defined as "a"
              a := b;
              fa := fb;
              b := b + (if abs(d) > tol then d else if m > 0 then tol else -tol);
              fb := normalizationResidue(c1,c2,b);

              if fb > 0 and fc > 0 or fb < 0 and fc < 0 then
                // initialize variables
                c := a;
                fc := fa;
                e := b - a;
                d := e;
              end if;
            end if;
          end while;

          annotation (Documentation(info="<html>

<p>
This function determines the solution of <b>one non-linear algebraic equation</b> \"y=f(u)\"
in <b>one unknown</b> \"u\" in a reliable way. It is one of the best numerical
algorithms for this purpose. As input, the nonlinear function f(u)
has to be given, as well as an interval u_min, u_max that
contains the solution, i.e., \"f(u_min)\" and \"f(u_max)\" must
have a different sign. If possible, a smaller interval is computed by
inverse quadratic interpolation (interpolating with a quadratic polynomial
through the last 3 points and computing the zero). If this fails,
bisection is used, which always reduces the interval by a factor of 2.
The inverse quadratic interpolation method has superlinear convergence.
This is roughly the same convergence rate as a globally convergent Newton
method, but without the need to compute derivatives of the non-linear
function. The solver function is a direct mapping of the Algol 60 procedure
\"zero\" to Modelica, from:
</p>

<dl>
<dt> Brent R.P.:</dt>
<dd> <b>Algorithms for Minimization without derivatives</b>.
     Prentice Hall, 1973, pp. 58-59.</dd>
</dl>

</html>"));
        end solveOneNonlinearEquation;

        algorithm
           // Find interval for alpha
           (alpha_min, alpha_max) :=findInterval(c1, c2);

           // Compute alpha, so that abs(G(p)) = -3db
           alpha :=solveOneNonlinearEquation(
            c1,
            c2,
            alpha_min,
            alpha_max);
        end normalizationFactor;

        encapsulated function bandPassAlpha "Return alpha for band pass"
          extends Modelica.Icons.Function;

          import Modelica;
           input Real a "Coefficient of s^1";
           input Real b "Coefficient of s^0";
           input Modelica.SIunits.AngularVelocity w
            "Bandwidth angular frequency";
           output Real alpha "Alpha factor to build up band pass";

        protected
          Real alpha_min;
          Real alpha_max;
          Real z_min;
          Real z_max;
          Real z;

          function residue "Residue of non-linear equation"
            extends Modelica.Icons.Function;
            input Real a;
            input Real b;
            input Real w;
            input Real z;
            output Real res;
          algorithm
            res := z^2 + (a*w*z/(1+z))^2 - (2+b*w^2)*z + 1;
          end residue;

        function solveOneNonlinearEquation
            "Solve f(u) = 0; f(u_min) and f(u_max) must have different signs"
            extends Modelica.Icons.Function;
            import Modelica.Utilities.Streams.error;

          input Real aa;
          input Real bb;
          input Real ww;
          input Real u_min "Lower bound of search interval";
          input Real u_max "Upper bound of search interval";
          input Real tolerance=100*Modelica.Constants.eps
              "Relative tolerance of solution u";
          output Real u "Value of independent variable so that f(u) = 0";

          protected
          constant Real eps=Modelica.Constants.eps "machine epsilon";
          Real a=u_min "Current best minimum interval value";
          Real b=u_max "Current best maximum interval value";
          Real c "Intermediate point a <= c <= b";
          Real d;
          Real e "b - a";
          Real m;
          Real s;
          Real p;
          Real q;
          Real r;
          Real tol;
          Real fa "= f(a)";
          Real fb "= f(b)";
          Real fc;
          Boolean found=false;
        algorithm
          // Check that f(u_min) and f(u_max) have different sign
          fa := residue(aa,bb,ww,u_min);
          fb := residue(aa,bb,ww,u_max);
          fc := fb;
          if fa > 0.0 and fb > 0.0 or fa < 0.0 and fb < 0.0 then
            error(
              "The arguments u_min and u_max to solveOneNonlinearEquation(..)\n" +
              "do not bracket the root of the single non-linear equation:\n" +
              "  u_min  = " + String(u_min) + "\n" + "  u_max  = " + String(u_max)
               + "\n" + "  fa = f(u_min) = " + String(fa) + "\n" +
              "  fb = f(u_max) = " + String(fb) + "\n" +
              "fa and fb must have opposite sign which is not the case");
          end if;

          // Initialize variables
          c := a;
          fc := fa;
          e := b - a;
          d := e;

          // Search loop
          while not found loop
            if abs(fc) < abs(fb) then
              a := b;
              b := c;
              c := a;
              fa := fb;
              fb := fc;
              fc := fa;
            end if;

            tol := 2*eps*abs(b) + tolerance;
            m := (c - b)/2;

            if abs(m) <= tol or fb == 0.0 then
              // root found (interval is small enough)
              found := true;
              u := b;
            else
              // Determine if a bisection is needed
              if abs(e) < tol or abs(fa) <= abs(fb) then
                e := m;
                d := e;
              else
                s := fb/fa;
                if a == c then
                  // linear interpolation
                  p := 2*m*s;
                  q := 1 - s;
                else
                  // inverse quadratic interpolation
                  q := fa/fc;
                  r := fb/fc;
                  p := s*(2*m*q*(q - r) - (b - a)*(r - 1));
                  q := (q - 1)*(r - 1)*(s - 1);
                end if;

                if p > 0 then
                  q := -q;
                else
                  p := -p;
                end if;

                s := e;
                e := d;
                if 2*p < 3*m*q - abs(tol*q) and p < abs(0.5*s*q) then
                  // interpolation successful
                  d := p/q;
                else
                  // use bi-section
                  e := m;
                  d := e;
                end if;
              end if;

              // Best guess value is defined as "a"
              a := b;
              fa := fb;
              b := b + (if abs(d) > tol then d else if m > 0 then tol else -tol);
              fb := residue(aa,bb,ww,b);

              if fb > 0 and fc > 0 or fb < 0 and fc < 0 then
                // initialize variables
                c := a;
                fc := fa;
                e := b - a;
                d := e;
              end if;
            end if;
          end while;

          annotation (Documentation(info="<html>

<p>
This function determines the solution of <b>one non-linear algebraic equation</b> \"y=f(u)\"
in <b>one unknown</b> \"u\" in a reliable way. It is one of the best numerical
algorithms for this purpose. As input, the nonlinear function f(u)
has to be given, as well as an interval u_min, u_max that
contains the solution, i.e., \"f(u_min)\" and \"f(u_max)\" must
have a different sign. If possible, a smaller interval is computed by
inverse quadratic interpolation (interpolating with a quadratic polynomial
through the last 3 points and computing the zero). If this fails,
bisection is used, which always reduces the interval by a factor of 2.
The inverse quadratic interpolation method has superlinear convergence.
This is roughly the same convergence rate as a globally convergent Newton
method, but without the need to compute derivatives of the non-linear
function. The solver function is a direct mapping of the Algol 60 procedure
\"zero\" to Modelica, from:
</p>

<dl>
<dt> Brent R.P.:</dt>
<dd> <b>Algorithms for Minimization without derivatives</b>.
     Prentice Hall, 1973, pp. 58-59.</dd>
</dl>

</html>"));
        end solveOneNonlinearEquation;

        algorithm
          assert( a^2/4 - b <= 0,  "Band pass transformation cannot be computed");
          z :=solveOneNonlinearEquation(a, b, w, 0, 1);
          alpha := sqrt(z);

          annotation (Documentation(info="<html>
<p>
A band pass with bandwidth \"w\" is determined from a low pass
</p>

<pre>
  1/(p^2 + a*p + b)
</pre>

<p>
with the transformation
</p>

<pre>
  new(p) = (p + 1/p)/w
</pre>

<p>
This results in the following derivation:
</p>

<pre>
  1/(p^2 + a*p + b) -> 1/( (p+1/p)^2/w^2 + a*(p + 1/p)/w + b )
                     = 1 /( ( p^2 + 1/p^2 + 2)/w^2 + (p + 1/p)*a/w + b )
                     = w^2*p^2 / (p^4 + 2*p^2 + 1 + (p^3 + p)a*w + b*w^2*p^2)
                     = w^2*p^2 / (p^4 + a*w*p^3 + (2+b*w^2)*p^2 + a*w*p + 1)
</pre>

<p>
This 4th order transfer function shall be split in to two transfer functions of order 2 each
for numerical reasons. With the following formulation, the fourth order
polynomial can be represented (with the unknowns \"c\" and \"alpha\"):
</p>

<pre>
  g(p) = w^2*p^2 / ( (p*alpha)^2 + c*(p*alpha) + 1) * ( (p/alpha)^2 + c*(p/alpha) + 1)
       = w^2*p^2 / ( p^4 + c*(alpha + 1/alpha)*p^3 + (alpha^2 + 1/alpha^2 + c^2)*p^2
                                                   + c*(alpha + 1/alpha)*p + 1 )
</pre>

<p>
Comparison of coefficients:
</p>

<pre>
  c*(alpha + 1/alpha) = a*w           -> c = a*w / (alpha + 1/alpha)
  alpha^2 + 1/alpha^2 + c^2 = 2+b*w^2 -> equation to determine alpha

  alpha^4 + 1 + a^2*w^2*alpha^4/(1+alpha^2)^2 = (2+b*w^2)*alpha^2
    or z = alpha^2
  z^2 + a^2*w^2*z^2/(1+z)^2 - (2+b*w^2)*z + 1 = 0
</pre>

<p>
Therefore the last equation has to be solved for \"z\" (basically, this means to compute
a real zero of a fourth order polynomial):
</p>

<pre>
   solve: 0 = f(z)  = z^2 + a^2*w^2*z^2/(1+z)^2 - (2+b*w^2)*z + 1  for \"z\"
              f(0)  = 1  &gt; 0
              f(1)  = 1 + a^2*w^2/4 - (2+b*w^2) + 1
                    = (a^2/4 - b)*w^2  &lt; 0
                    // since b - a^2/4 > 0 requirement for complex conjugate poles
   -> 0 &lt; z &lt; 1
</pre>

<p>
This function computes the solution of this equation and returns \"alpha = sqrt(z)\";
</p>

</html>"));
        end bandPassAlpha;
      end Utilities;
    end Filter;
  end Internal;
  annotation (
    Documentation(info="<html>
<p>
This package contains basic <b>continuous</b> input/output blocks
described by differential equations.
</p>

<p>
All blocks of this package can be initialized in different
ways controlled by parameter <b>initType</b>. The possible
values of initType are defined in
<a href=\"modelica://Modelica.Blocks.Types.Init\">Modelica.Blocks.Types.Init</a>:
</p>

<table border=1 cellspacing=0 cellpadding=2>
  <tr><td valign=\"top\"><b>Name</b></td>
      <td valign=\"top\"><b>Description</b></td></tr>

  <tr><td valign=\"top\"><b>Init.NoInit</b></td>
      <td valign=\"top\">no initialization (start values are used as guess values with fixed=false)</td></tr>

  <tr><td valign=\"top\"><b>Init.SteadyState</b></td>
      <td valign=\"top\">steady state initialization (derivatives of states are zero)</td></tr>

  <tr><td valign=\"top\"><b>Init.InitialState</b></td>
      <td valign=\"top\">Initialization with initial states</td></tr>

  <tr><td valign=\"top\"><b>Init.InitialOutput</b></td>
      <td valign=\"top\">Initialization with initial outputs (and steady state of the states if possible)</td></tr>
</table>

<p>
For backward compatibility reasons the default of all blocks is
<b>Init.NoInit</b>, with the exception of Integrator and LimIntegrator
where the default is <b>Init.InitialState</b> (this was the initialization
defined in version 2.2 of the Modelica standard library).
</p>

<p>
In many cases, the most useful initial condition is
<b>Init.SteadyState</b> because initial transients are then no longer
present. The drawback is that in combination with a non-linear
plant, non-linear algebraic equations occur that might be
difficult to solve if appropriate guess values for the
iteration variables are not provided (i.e., start values with fixed=false).
However, it is often already useful to just initialize
the linear blocks from the Continuous blocks library in SteadyState.
This is uncritical, because only linear algebraic equations occur.
If Init.NoInit is set, then the start values for the states are
interpreted as <b>guess</b> values and are propagated to the
states with fixed=<b>false</b>.
</p>

<p>
Note, initialization with Init.SteadyState is usually difficult
for a block that contains an integrator
(Integrator, LimIntegrator, PI, PID, LimPID).
This is due to the basic equation of an integrator:
</p>

<pre>
  <b>initial equation</b>
     <b>der</b>(y) = 0;   // Init.SteadyState
  <b>equation</b>
     <b>der</b>(y) = k*u;
</pre>

<p>
The steady state equation leads to the condition that the input to the
integrator is zero. If the input u is already (directly or indirectly) defined
by another initial condition, then the initialization problem is <b>singular</b>
(has none or infinitely many solutions). This situation occurs often
for mechanical systems, where, e.g., u = desiredSpeed - measuredSpeed and
since speed is both a state and a derivative, it is always defined by
Init.InitialState or Init.SteadyState initialization.
</p>

<p>
In such a case, <b>Init.NoInit</b> has to be selected for the integrator
and an additional initial equation has to be added to the system
to which the integrator is connected. E.g., useful initial conditions
for a 1-dim. rotational inertia controlled by a PI controller are that
<b>angle</b>, <b>speed</b>, and <b>acceleration</b> of the inertia are zero.
</p>

</html>"), Icon(graphics={Line(
          origin={0.061,4.184},
          points={{81.939,36.056},{65.362,36.056},{14.39,-26.199},{-29.966,
              113.485},{-65.374,-61.217},{-78.061,-78.184}},
          color={95,95,95},
          smooth=Smooth.Bezier)}));
end Continuous;
