within ;
package ObsoleteModelica3
  "Library that contains components from Modelica Standard Library 2.2.2 that have been removed from version 3.0"

  package Blocks
    "Library of basic input/output control blocks (continuous, discrete, logical, table blocks)"
    package Interfaces
      "Library of connectors and partial models for input/output blocks"
    connector RealSignal = Real "Real port (both input/output possible)"
        annotation (obsolete="Connector is not valid according to Modelica 3, since input/output prefixes are missing. When using this connector, it is not possible to check for balanced models.",
    Documentation(info="<html>
<p>
Connector with one signal of type Real (no icon, no input/output prefix).
</p>
</html>"));
    connector BooleanSignal = Boolean
        "Boolean port (both input/output possible)"
        annotation (obsolete="Connector is not valid according to Modelica 3, since input/output prefixes are missing. When using this connector, it is not possible to check for balanced models.",
    Documentation(info="<html>
<p>
Connector with one signal of type Boolean (no icon, no input/output prefix).
</p>
</html>"));
    connector IntegerSignal = Integer
        "Integer port (both input/output possible)"
        annotation (obsolete="Connector is not valid according to Modelica 3, since input/output prefixes are missing. When using this connector, it is not possible to check for balanced models.",
    Documentation(info="<html>
<p>
Connector with one signal of type .
</p>
</html>"));
      package Adaptors
        "Obsolete package with components to send signals to a bus or receive signals from a bus (only for backward compatibility)"
      model AdaptorReal
          "Completely obsolete adaptor between 'old' and 'new' Real signal connectors (only for backward compatibility)"
        extends ObsoleteModelica3.Icons.ObsoleteBlock;
        ObsoleteModelica3.Blocks.Interfaces.RealSignal newReal
            "Connector of Modelica version 2.1"                annotation (                            Hide=true,
              Placement(transformation(extent={{100,-10},{120,10}})));
        RealPort oldReal(final n=1) "Connector of Modelica version 1.6" annotation (Placement(
                transformation(extent={{-120,-10},{-100,10}})));

        protected
        connector RealPort "Connector with signals of type Real"
          parameter Integer n=1 "Dimension of signal vector" annotation (Hide=true);
          replaceable type SignalType = Real "type of signal";
          SignalType signal[n] "Real signals" annotation (Hide=true);

        end RealPort;
      equation
        newReal = oldReal.signal[1];
        annotation(defaultConnectionStructurallyInconsistent=true,
          obsolete="Model is not balanced, so equation check will not work. This model is no longer needed",
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Rectangle(
                  extent={{-100,40},{100,-40}},
                  lineColor={0,0,255},
                  fillColor={255,255,255},
                  fillPattern=FillPattern.Solid),
                Text(
                  extent={{-144,96},{144,46}},
                  lineColor={0,0,0},
                  textString=""),
                Text(
                  extent={{-88,22},{88,-24}},
                  lineColor={0,0,255},
                  textString="adaptor"),
                Text(
                  extent={{-216,-58},{36,-80}},
                  lineColor={0,0,0},
                  textString="port.signal")}),
                                      Documentation(info="<html>
<p>
Completely obsolete adaptor between the Real signal connector
of version 1.6 and version &ge; 2.1 of the Modelica Standard Library.
This block is only provided for backward compatibility.
</p>
</html>"));
      end AdaptorReal;

      model AdaptorBoolean
          "Completely obsolete adaptor between 'old' and 'new' Boolean signal connectors (only for backward compatibility)"
        extends ObsoleteModelica3.Icons.ObsoleteBlock;
        ObsoleteModelica3.Blocks.Interfaces.BooleanSignal newBoolean
            "Connector of Modelica version 2.1"
          annotation (                            Hide=true, Placement(
                transformation(extent={{100,-10},{120,10}})));
        BooleanPort oldBoolean(final n=1) "Connector of Modelica version 1.6" annotation (Placement(
                transformation(extent={{-120,-10},{-100,10}})));

        protected
        connector BooleanPort "Connector with signals of type Boolean"
          parameter Integer n=1 "Dimension of signal vector" annotation (Hide=true);
          replaceable type SignalType = Boolean "type of signal";
          SignalType signal[n] "Boolean signals" annotation (Hide=true);

        end BooleanPort;
      equation

        newBoolean = oldBoolean.signal[1];

        annotation(defaultConnectionStructurallyInconsistent=true,
          obsolete="Model is not balanced, so equation check will not work. This model is no longer needed",
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Rectangle(
                  extent={{-100,40},{100,-40}},
                  lineColor={255,0,255},
                  fillColor={255,255,255},
                  fillPattern=FillPattern.Solid),
                Text(
                  extent={{-144,96},{144,46}},
                  lineColor={0,0,0},
                  textString=""),
                Text(
                  extent={{-88,22},{88,-24}},
                  lineColor={255,0,255},
                  textString="adaptor"),
                Text(
                  extent={{-216,-58},{36,-80}},
                  lineColor={0,0,0},
                  textString="port.signal")}),
                                      Documentation(info="<html>
<p>
Completely obsolete adaptor between the Real signal connector
of version 1.6 and version &ge; 2.1 of the Modelica Standard Library.
This block is only provided for backward compatibility.
</p>
</html>"));
      end AdaptorBoolean;

      model AdaptorInteger
          "Completely obsolete adaptor between 'old' and 'new' Integer signal connectors (only for backward compatibility)"
        extends ObsoleteModelica3.Icons.ObsoleteBlock;
        ObsoleteModelica3.Blocks.Interfaces.IntegerSignal newInteger
            "Connector of Modelica version 2.1"
          annotation (                            Hide=true, Placement(
                transformation(extent={{100,-10},{120,10}})));
        IntegerPort oldInteger(final n=1) "Connector of Modelica version 1.6"  annotation (Placement(
                transformation(extent={{-120,-10},{-100,10}})));

        protected
        connector IntegerPort "Connector with signals of type Integer"
          parameter Integer n=1 "Dimension of signal vector" annotation (Hide=true);
          replaceable type SignalType = Integer "type of signal";
          SignalType signal[n] "Integer signals" annotation (Hide=true);

        end IntegerPort;
      equation

        newInteger = oldInteger.signal[1];

        annotation(defaultConnectionStructurallyInconsistent=true,
          obsolete="Model is not balanced, so equation check will not work. This model is no longer needed",
           Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Rectangle(
                  extent={{-100,40},{100,-40}},
                  lineColor={255,127,0},
                  fillColor={255,255,255},
                  fillPattern=FillPattern.Solid),
                Text(
                  extent={{-144,96},{144,46}},
                  lineColor={0,0,0},
                  textString=""),
                Text(
                  extent={{-88,22},{88,-24}},
                  lineColor={255,127,0},
                  textString="adaptor"),
                Text(
                  extent={{-216,-58},{36,-80}},
                  lineColor={0,0,0},
                  textString="port.signal")}),
                                      Documentation(info="<html>
<p>
Completely obsolete adaptor between the Real signal connector
of version 1.6 and version &ge; 2.1 of the Modelica Standard Library.
This block is only provided for backward compatibility.
</p>
</html>"));
      end AdaptorInteger;
      end Adaptors;
    end Interfaces;

    package Math "Library of mathematical functions as input/output blocks"
      package UnitConversions
        "Conversion blocks to convert between SI and non-SI unit signals"
        block ConvertAllUnits
          "Obsolete block. Use one of Modelica.Blocks.Math.UnitConversions.XXX instead"
          replaceable block ConversionBlock =
              Modelica.Blocks.Interfaces.PartialConversionBlock
            "Conversion block"
            annotation (choicesAllMatching=true,
            Documentation(info=
                           "<html>
<p>
Internal replaceable block that is used to construct the
\"pull down menu\" of the available unit conversions.
</p>
</html>"));
          extends ConversionBlock;
          extends ObsoleteModelica3.Icons.ObsoleteBlock;

          annotation (
            obsolete="Model is not according to Modelica Language 3.0 since replaceable base class present. " +
                              "Use instead one of Modelica.Blocks.Math.UnitConversions.XXX",
            defaultComponentName="convert",
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={Line(points={{-90,0},{30,0}}, color=
                      {191,0,0}), Polygon(
                  points={{90,0},{30,20},{30,-20},{90,0}},
                  lineColor={191,0,0},
                  fillColor={191,0,0},
                  fillPattern=FillPattern.Solid)}),
            Documentation(info="<html>
<p>This block implements the Modelica.SIunits.Conversions functions as a fixed causality block to
simplify their use. The block contains a replaceable block class <b>ConversionBlock</b> that can be
changed to be any of the blocks defined in Modelica.Blocks.Math.UnitConversions, and more generally, any
blocks that extend from Modelica.Blocks.Interfaces.PartialConversionBlock.
</p>

<p>
The desired conversion can be selected in the parameter menu
(the selected units are then displayed in the icon):
</p>

<p>
<img src=\"modelica://Modelica/Resources/Images/Blocks/ConvertAllUnits.png\">
</p>

</html>"));
        end ConvertAllUnits;
      end UnitConversions;

      block TwoInputs
        "Obsolete block. Use instead Modelica.Blocks.Math.InverseBlockConstraints"
        extends Modelica.Blocks.Interfaces.BlockIcon;
        extends ObsoleteModelica3.Icons.ObsoleteBlock;
            Modelica.Blocks.Interfaces.RealInput u1
          "Connector of first Real input signal"
              annotation (                                       layer="icon",
            Placement(transformation(extent={{-139.742,-19.0044},{-100,20}})));
            Modelica.Blocks.Interfaces.RealInput u2
          "Connector of second Real input signal (u1=u2)"
                                           annotation (
              layer="icon", Placement(transformation(
              origin={120,0},
              extent={{-20,-20},{20,20}},
              rotation=180)));
      equation
            u1 = u2;
            annotation(defaultConnectionStructurallyInconsistent=true,
              obsolete="Model is not balanced, i.e., not according to Modelica Language 3.0. Use instead Modelica.Blocks.Math.InverseBlockConstraints",
              Documentation(info="<HTML>
<p>
This block is used to enable assignment of values to variables preliminary
defined as outputs (e.g., useful for inverse model generation).
</p>

</html>"),           Icon(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}}),
                graphics={Text(
                extent={{-95,50},{95,-50}},
                lineColor={0,0,127},
                textString="=")}));
      end TwoInputs;

          block TwoOutputs
        "Obsolete block. Use instead Modelica.Blocks.Math.InverseBlockConstraints"
            extends Modelica.Blocks.Interfaces.BlockIcon;
            extends ObsoleteModelica3.Icons.ObsoleteBlock;
            output Modelica.Blocks.Interfaces.RealOutput y1
          "Connector of first Real output signal"
              annotation (Placement(transformation(extent={{100,-10},{120,10}})));
            output Modelica.Blocks.Interfaces.RealOutput y2
          "Connector of second Real output signal (y1=y2)"
                                                   annotation (Placement(
              transformation(
              origin={-110.366,-0.90289},
              extent={{-10.0005,-10},{10.0005,10}},
              rotation=180)));
          equation
            y1 = y2;
            annotation(defaultConnectionStructurallyInconsistent=true,
              obsolete="Model is not balanced, i.e., not according to Modelica Language 3.0. Use instead Modelica.Blocks.Math.InverseBlockConstraints",
              Documentation(info="<html>
<p>
This block is used to enable calculation of values preliminary defined as inputs.
(e.g., useful for inverse model generation).
</p>

</html>"),           Icon(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}}),
                graphics={Text(
                extent={{-95,50},{95,-50}},
                lineColor={0,0,127},
                textString="=")}));
          end TwoOutputs;
    end Math;

  end Blocks;

  package Electrical
    "Library of electrical models (analog, digital, machines, multi-phase)"
    package Analog "Library for analog electrical models"
      package Basic
        "Basic electrical components such as resistor, capacitor, transformer"
        model HeatingResistor
          "Obsolete model. Use Modelica.Electrical.Analog.Basic.HeatingResistor instead"
          extends Modelica.Electrical.Analog.Interfaces.OnePort;
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          parameter Modelica.SIunits.Resistance R_ref=1
            "Resistance at temperature T_ref";
          parameter Modelica.SIunits.Temperature T_ref=300
            "Reference temperature";
          parameter Real alpha(unit="1/K") = 0
            "Temperature coefficient of resistance";

          Modelica.SIunits.Resistance R
            "Resistance = R_ref*(1 + alpha*(heatPort.T - T_ref));";

          Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a heatPort annotation (Placement(
                transformation(
                origin={0,-100},
                extent={{10,-10},{-10,10}},
                rotation=270)));
        equation
          v = R*i;

          if cardinality(heatPort) > 0 then
            R = R_ref*(1 + alpha*(heatPort.T - T_ref));
            heatPort.Q_flow = -v*i;
          else
            /* heatPort is not connected resulting in the
         implicit equation 'heatPort.Q_flow = 0'
      */
            R = R_ref;
            heatPort.T = T_ref;
          end if;
          annotation (
            obsolete="Model equations depend on cardinality(..) which will become obsolete in the Modelica language. Use instead Modelica.Electrical.Analog.Basic.HeatingResistor",
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics={
                Line(points={{-110,20},{-85,20}}, color={160,160,164}),
                Polygon(
                  points={{-95,23},{-85,20},{-95,17},{-95,23}},
                  lineColor={160,160,164},
                  fillColor={160,160,164},
                  fillPattern=FillPattern.Solid),
                Line(points={{90,20},{115,20}}, color={160,160,164}),
                Line(points={{-125,0},{-115,0}}, color={160,160,164}),
                Line(points={{-120,-5},{-120,5}}, color={160,160,164}),
                Text(
                  extent={{-110,25},{-90,45}},
                  lineColor={160,160,164},
                  textString="i"),
                Polygon(
                  points={{105,23},{115,20},{105,17},{105,23}},
                  lineColor={160,160,164},
                  fillColor={160,160,164},
                  fillPattern=FillPattern.Solid),
                Line(points={{115,0},{125,0}}, color={160,160,164}),
                Text(
                  extent={{90,45},{110,25}},
                  lineColor={160,160,164},
                  textString="i"),
                Rectangle(extent={{-70,30},{70,-30}}),
                Line(points={{-96,0},{-70,0}}),
                Line(points={{70,0},{96,0}}),
                Line(points={{0,-30},{0,-90}}, color={191,0,0}),
                Line(points={{-52,-50},{48,50}}, color={0,0,255}),
                Polygon(
                  points={{40,52},{50,42},{54,56},{40,52}},
                  lineColor={0,0,255},
                  fillColor={0,0,255},
                  fillPattern=FillPattern.Solid)}),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Text(extent={{-142,60},{143,118}}, textString="%name"),
                Line(points={{-90,0},{-70,0}}),
                Line(points={{70,0},{90,0}}),
                Rectangle(
                  extent={{-70,30},{70,-30}},
                  lineColor={0,0,255},
                  fillColor={255,255,255},
                  fillPattern=FillPattern.Solid),
                Line(points={{0,-30},{0,-91}}, color={191,0,0}),
                Line(points={{-52,-50},{48,50}}, color={0,0,255}),
                Polygon(
                  points={{40,52},{50,42},{54,56},{40,52}},
                  lineColor={0,0,255},
                  fillColor={0,0,255},
                  fillPattern=FillPattern.Solid)}),
            Documentation(info="<HTML>
<p>This is a model for an electrical resistor where the generated heat
is dissipated to the environment via connector <b>heatPort</b> and where
the resistance R is temperature dependent according to the following
equation:</p>
<pre>    R = R_ref*(1 + alpha*(heatPort.T - T_ref))
</pre>
<p><b>alpha</b> is the <b>temperature coefficient of resistance</b>, which
is often abbreviated as <b>TCR</b>. In resistor catalogues, it is usually
defined as <b>X [ppm/K]</b> (parts per million, similarly to percentage)
meaning <b>X*1.e-6 [1/K]</b>. Resistors are available for 1 .. 7000 ppm/K,
i.e., alpha = 1e-6 .. 7e-3 1/K;</p>
<p>When connector <b>heatPort</b> is <b>not</b> connected, the temperature
dependent behaviour is switched off by setting heatPort.T = T_ref.
Additionally, the equation <code>heatPort.Q_flow = 0</code> is implicitly present
due to a special rule in Modelica that flow variables of not connected
connectors are set to zero.</p>
</html>",         revisions=
                 "<html>
<ul>
<li><i> 2002   </i>
       by Anton Haumer<br> initially implemented<br>
       </li>
</ul>
</html>"));
        end HeatingResistor;
      end Basic;
    end Analog;
  end Electrical;

  package Icons "Library of icons"
    partial block ObsoleteBlock
      "Icon for an obsolete block (use only for this case)"

      annotation (obsolete="Only used to mark an obsolete block. Do not use otherwise.",
              Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                -100},{100,100}}), graphics={Rectangle(
              extent={{-102,102},{102,-102}},
              lineColor={255,0,0},
              pattern=LinePattern.Dash,
              lineThickness=0.5)}),        Documentation(info="<html>
<p>
This partial block is intended to provide a <u>default icon
for an obsolete block</u> that will be removed from the
corresponding library in a future release.
</p>
</html>"));
    end ObsoleteBlock;

    partial model ObsoleteModel
      "Icon for an obsolete model (use only for this case)"

      annotation (obsolete="Only used to mark an obsolete model. Do not use otherwise.",
              Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                -100},{100,100}}), graphics={Rectangle(
              extent={{-102,102},{102,-102}},
              lineColor={255,0,0},
              pattern=LinePattern.Dash,
              lineThickness=0.5)}),        Documentation(info="<html>
<p>
This partial model is intended to provide a <u>default icon
for an obsolete model</u> that will be removed from the
corresponding library in a future release.
</p>
</html>"));
    end ObsoleteModel;

    partial class Enumeration
      "Obsolete class (icon for an enumeration emulated by a package). Use a real enumeration instead"

      annotation (obsolete="Icon for an emulated enumeration. Emulated enumerations are no longer used (only real enumerations)",
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                {100,100}}), graphics={
            Text(extent={{-138,164},{138,104}}, textString="%name"),
            Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={255,0,127},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,100},{100,-100}},
              lineColor={255,0,127},
              textString="e")}),
                          Documentation(info="<html>
<p>
This icon is designed for an <b>enumeration</b>
(that is emulated by a package).
</p>
</html>"));
    end Enumeration;
  end Icons;

  package Mechanics
    "Library of 1-dim. and 3-dim. mechanical components (multi-body, rotational, translational)"

    package MultiBody "Library to model 3-dimensional mechanical systems"
      package Forces
        "Components that exert forces and/or torques between frames"
        model WorldForceAndTorque
          "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Forces.WorldForceAndTorque"

          import SI = Modelica.SIunits;
          import Modelica.Mechanics.MultiBody.Types;
          extends Modelica.Mechanics.MultiBody.Interfaces.PartialOneFrame_b;
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.Blocks.Interfaces.RealInput load[6]
            "[1:6] = x-, y-, z-coordinates of force and x-, y-, z-coordiantes of torque resolved in world frame"
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
          parameter Boolean animation=true
            "= true, if animation shall be enabled";
          parameter Real N_to_m(unit="N/m") = world.defaultN_to_m
            " Force arrow scaling (length = force/N_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          parameter Real Nm_to_m(unit="N.m/m") = world.defaultNm_to_m
            " Torque arrow scaling (length = torque/Nm_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter forceDiameter=world.defaultArrowDiameter
            " Diameter of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter torqueDiameter=forceDiameter
            " Diameter of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color forceColor=Modelica.Mechanics.MultiBody.Types.Defaults.ForceColor
            " Color of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color torqueColor=Modelica.Mechanics.MultiBody.Types.Defaults.TorqueColor
            " Color of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.SpecularCoefficient specularCoefficient = world.defaultSpecularCoefficient
            "Reflection of ambient light (= 0: light is completely absorbed)"
            annotation (Dialog(group="if animation = true", enable=animation));

        protected
          SI.Position f_in_m[3]=frame_b.f/N_to_m
            "Force mapped from N to m for animation";
          SI.Position t_in_m[3]=frame_b.t/Nm_to_m
            "Torque mapped from Nm to m for animation";
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Arrow forceArrow(
            diameter=forceDiameter,
            color=forceColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=f_in_m,
            r_head=-f_in_m) if world.enableAnimation and animation;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.DoubleArrow
            torqueArrow(
            diameter=torqueDiameter,
            color=torqueColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=t_in_m,
            r_head=-t_in_m) if world.enableAnimation and animation;
        equation
          frame_b.f = -Modelica.Mechanics.MultiBody.Frames.resolve2(frame_b.R, load[1
            :3]);
          frame_b.t = -Modelica.Mechanics.MultiBody.Frames.resolve2(frame_b.R, load[4
            :6]);
          annotation (
            obsolete="Based on a packed result signal which is not a good design. Use instead Modelica.Mechanics.MultiBody.Forces.WorldForceAndTorque",
            preferredView="info",
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics={
                Polygon(
                  points={{-100,10},{50,10},{50,31},{97,0},{50,-31},{50,-10},{-100,
                      -10},{-100,10}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(
                  points={{-100,11},{-94,24},{-86,39},{-74,59},{-65,71},{-52,83},
                      {-35,92},{-22,95},{-8,95},{7,91},{19,84},{32,76},{44,66},
                      {52,58},{58,51}},
                  thickness=0.5),
                Polygon(
                  points={{97,18},{72,77},{38,42},{97,18}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid)}),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Text(
                  extent={{-74,62},{44,24}},
                  lineColor={192,192,192},
                  textString="world"),
                Polygon(
                  points={{-100,10},{50,10},{50,31},{94,0},{50,-31},{50,-10},{-100,
                      -10},{-100,10}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Text(extent={{-137,-47},{148,-108}}, textString="%name"),
                Line(
                  points={{-98,14},{-92,27},{-84,42},{-72,62},{-63,74},{-50,86},
                      {-33,95},{-20,98},{-6,98},{9,94},{21,87},{34,79},{46,69},
                      {54,61},{60,54}},
                  thickness=0.5),
                Polygon(
                  points={{99,21},{74,80},{40,45},{99,21}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid)}),
            Documentation(info="<HTML>
<p>
The <b>6</b> signals of the <b>load</b> connector are interpreted
as the x-, y- and z-coordinates of a <b>force</b> and as
the x-, y-, and z-coordinates of a <b>torque</b> resolved in the
<b>world frame</b> and acting at the frame connector to which this
component is attached. The input signals are mapped to the force
and torque in the following way:
</p>
<pre>
   force  = load[1:3]
   torque = load[4:6]
</pre>
<p>
The force and torque are by default visualized as an arrow (force)
and as a double arrow (torque) acting at the connector to which
they are connected. The diameters
and colors of the arrows are fixed and can be defined via
parameters <b>forceDiameter</b>, <b>torqueDiameter</b>,
<b>forceColor</b> and <b>torqueColor</b>. The arrows
point in the directions defined by the
inPort.signal signals. The lengths of the arrows are proportional
to the length of the force and torque vectors, respectively, using parameters
<b>N_to_m</b> and <b>Nm_to_m</b> as scaling factors. For example, if N_to_m = 100 N/m,
then a force of 350 N is displayed as an arrow of length 3.5 m.
</p>
<p>
An example how to use this model is given in the
following figure:
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Forces/WorldForceAndTorque1.png\">

<p>
This leads to the following animation
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Forces/WorldForceAndTorque2.png\">
</html>"));
        end WorldForceAndTorque;

        model FrameForceAndTorque
          "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Forces.ForceAndTorque"

          import SI = Modelica.SIunits;
          import Modelica.Mechanics.MultiBody.Types;
          extends Modelica.Mechanics.MultiBody.Interfaces.PartialOneFrame_b;
          extends ObsoleteModelica3.Icons.ObsoleteModel;
          Modelica.Mechanics.MultiBody.Interfaces.Frame_resolve frame_resolve
            "If connected, the input signals are resolved in this frame"
            annotation (Placement(transformation(
                origin={0,100},
                extent={{16,-16},{-16,16}},
                rotation=270)));
          Modelica.Blocks.Interfaces.RealInput load[6]
            "[1:6] = x-, y-, z-coordinates of force and x-, y-, z-coordiantes of torque resolved in frame_b or frame_resolved (if connected)"
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
          parameter Boolean animation=true
            "= true, if animation shall be enabled";
          parameter Real N_to_m(unit="N/m") = world.defaultN_to_m
            " Force arrow scaling (length = force/N_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          parameter Real Nm_to_m(unit="N.m/m") = world.defaultNm_to_m
            " Torque arrow scaling (length = torque/Nm_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter forceDiameter=world.defaultArrowDiameter
            " Diameter of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter torqueDiameter=forceDiameter
            " Diameter of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color forceColor=Modelica.Mechanics.MultiBody.Types.Defaults.ForceColor
            " Color of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color torqueColor=Modelica.Mechanics.MultiBody.Types.Defaults.TorqueColor
            " Color of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.SpecularCoefficient specularCoefficient = world.defaultSpecularCoefficient
            "Reflection of ambient light (= 0: light is completely absorbed)"
            annotation (Dialog(group="if animation = true", enable=animation));

        protected
          SI.Position f_in_m[3]=frame_b.f/N_to_m
            "Force mapped from N to m for animation";
          SI.Position t_in_m[3]=frame_b.t/Nm_to_m
            "Torque mapped from Nm to m for animation";
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Arrow forceArrow(
            diameter=forceDiameter,
            color=forceColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=f_in_m,
            r_head=-f_in_m) if world.enableAnimation and animation;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.DoubleArrow
            torqueArrow(
            diameter=torqueDiameter,
            color=torqueColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=t_in_m,
            r_head=-t_in_m) if world.enableAnimation and animation;
        equation
          if cardinality(frame_resolve) == 0 then
            frame_b.f = -load[1:3];
            frame_b.t = -load[4:6];
            frame_resolve.r_0 = zeros(3);
            frame_resolve.R = Modelica.Mechanics.MultiBody.Frames.nullRotation();
          else
            frame_b.f = -Modelica.Mechanics.MultiBody.Frames.resolveRelative(
                load[1:3],
                frame_resolve.R,
                frame_b.R);
            frame_b.t = -Modelica.Mechanics.MultiBody.Frames.resolveRelative(
                load[4:6],
                frame_resolve.R,
                frame_b.R);
            frame_resolve.f = zeros(3);
            frame_resolve.t = zeros(3);
          end if;
          annotation (
            obsolete="Based on a packed result signal which is not a good design. Use instead Modelica.Mechanics.MultiBody.Forces.ForceAndTorque",
            preferredView="info",
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics={
                Polygon(
                  points={{-100,10},{50,10},{50,31},{97,0},{50,-31},{50,-10},{-100,
                      -10},{-100,10}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(points={{-100,11},{-94,24},{-86,39},{-74,59},{-65,71},{-52,
                      83},{-35,92},{-22,95},{-8,95},{7,91},{19,84},{32,76},{44,
                      66},{52,58},{58,51}}),
                Polygon(
                  points={{97,18},{72,77},{38,42},{97,18}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(
                  points={{0,97},{0,10}},
                  color={95,95,95},
                  pattern=LinePattern.Dot)}),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Text(
                  extent={{-74,62},{44,24}},
                  lineColor={192,192,192},
                  textString="resolve"),
                Polygon(
                  points={{-100,10},{50,10},{50,31},{94,0},{50,-31},{50,-10},{-100,
                      -10},{-100,10}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Text(extent={{-137,-47},{148,-108}}, textString="%name"),
                Line(points={{-100,10},{-92,26},{-84,42},{-76,52},{-60,68},{-46,
                      76},{-31,82},{-17,85},{-2,87},{14,86},{26,82},{37,75},{46,
                      69},{54,61},{60,54}}),
                Polygon(
                  points={{99,21},{74,80},{40,45},{99,21}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(
                  points={{0,95},{0,10}},
                  color={95,95,95},
                  pattern=LinePattern.Dot)}),
            Documentation(info="<HTML>
<p>
The <b>6</b> signals of the <b>load</b> connector are interpreted
as the x-, y- and z-coordinates of a <b>force</b> and as
the x-, y-, and z-coordinates of a <b>torque</b> acting at the frame
connector to which this component is attached. If connector
<b>frame_resolve</b> is <b>not</b> connected, the force and torque coordinates
are with respect to <b>frame_b</b>. If connector
<b>frame_resolve</b> is connected, the force and torque coordinates
are with respect to <b>frame_resolve</b>. In this case the
force and torque in connector frame_resolve are set to zero,
i.e., this connector is solely used to provide the information
of the coordinate system, in which the force coordinates
are defined. The input signals are mapped to the force
and torque in the following way:
</p>
<pre>
   force  = load[1:3]
   torque = load[4:6]
</pre>
<p>
The force and torque are by default visualized as an arrow (force)
and as a double arrow (torque) acting at the connector to which
they are connected. The diameters
and colors of the arrows are fixed and can be defined via
parameters <b>forceDiameter</b>, <b>torqueDiameter</b>,
<b>forceColor</b> and <b>torqueColor</b>. The arrows
point in the directions defined by the
inPort.signal signals. The lengths of the arrows are proportional
to the length of the force and torque vectors, respectively, using parameters
<b>N_to_m</b> and <b>Nm_to_m</b> as scaling factors. For example,
if N_to_m = 100 N/m,
then a force of 350 N is displayed as an arrow of length 3.5 m.
</p>
<p>
An example how to use this model is given in the
following figure:
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Forces/FrameForceAndTorque1.png\">

<p>
This leads to the following animation
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Forces/FrameForceAndTorque2.png\">
</html>"));
        end FrameForceAndTorque;

        model ForceAndTorque
          "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Forces.ForceAndTorque"

          import SI = Modelica.SIunits;
          import Modelica.Mechanics.MultiBody.Types;
          extends Modelica.Mechanics.MultiBody.Interfaces.PartialTwoFrames;
          extends ObsoleteModelica3.Icons.ObsoleteModel;
          Modelica.Mechanics.MultiBody.Interfaces.Frame_resolve frame_resolve
            "If connected, the input signals are resolved in this frame"
            annotation (Placement(transformation(
                origin={40,100},
                extent={{-16,-16},{16,16}},
                rotation=90)));

          Modelica.Blocks.Interfaces.RealInput load[6]
            "[1:6] = x-, y-, z-coordinates of force and x-, y-, z-coordiantes of torque resolved in frame_b or frame_resolved (if connected)"
            annotation (Placement(transformation(
                origin={-60,120},
                extent={{-20,-20},{20,20}},
                rotation=270)));
          parameter Boolean animation=true
            "= true, if animation shall be enabled";
          parameter Real N_to_m(unit="N/m") = world.defaultN_to_m
            " Force arrow scaling (length = force/N_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          parameter Real Nm_to_m(unit="N.m/m") = world.defaultNm_to_m
            " Torque arrow scaling (length = torque/Nm_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter forceDiameter=world.defaultArrowDiameter
            " Diameter of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter torqueDiameter=forceDiameter
            " Diameter of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter connectionLineDiameter=forceDiameter
            " Diameter of line connecting frame_a and frame_b"
            annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color forceColor=Modelica.Mechanics.MultiBody.Types.Defaults.ForceColor
            " Color of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color torqueColor=Modelica.Mechanics.MultiBody.Types.Defaults.TorqueColor
            " Color of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color connectionLineColor=Modelica.Mechanics.MultiBody.Types.Defaults.SensorColor
            " Color of line connecting frame_a and frame_b"
            annotation (Dialog(group="if animation = true", enable=animation));
          input Types.SpecularCoefficient specularCoefficient = world.defaultSpecularCoefficient
            "Reflection of ambient light (= 0: light is completely absorbed)"
            annotation (Dialog(group="if animation = true", enable=animation));
          SI.Position r_0[3]
            "Position vector from origin of frame_a to origin of frame_b resolved in world frame";
          SI.Force f_b_0[3] "frame_b.f resolved in world frame";
          SI.Torque t_b_0[3] "frame_b.t resolved in world frame";

        protected
          SI.Position f_in_m[3]=frame_b.f/N_to_m
            "Force mapped from N to m for animation";
          SI.Position t_in_m[3]=frame_b.t/Nm_to_m
            "Torque mapped from Nm to m for animation";
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Arrow forceArrow(
            diameter=forceDiameter,
            color=forceColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=f_in_m,
            r_head=-f_in_m) if world.enableAnimation and animation;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.DoubleArrow
            torqueArrow(
            diameter=torqueDiameter,
            color=torqueColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=t_in_m,
            r_head=-t_in_m) if world.enableAnimation and animation;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Shape
            connectionLine(
            shapeType="cylinder",
            lengthDirection=r_0,
            widthDirection={0,1,0},
            length=Modelica.Math.Vectors.length(              r_0),
            width=connectionLineDiameter,
            height=connectionLineDiameter,
            color=connectionLineColor,
            specularCoefficient=specularCoefficient,
            r=frame_a.r_0) if world.enableAnimation and animation;
        equation
          if cardinality(frame_resolve) == 0 then
            frame_b.f = -load[1:3];
            frame_b.t = -load[4:6];
            f_b_0 = Modelica.Mechanics.MultiBody.Frames.resolve1(frame_b.R, frame_b.f);
            t_b_0 = Modelica.Mechanics.MultiBody.Frames.resolve1(frame_b.R, frame_b.t);
            frame_resolve.r_0 = zeros(3);
            frame_resolve.R = Modelica.Mechanics.MultiBody.Frames.nullRotation();
          else
            f_b_0 = -Modelica.Mechanics.MultiBody.Frames.resolve1(frame_resolve.R,
              load[1:3]);
            t_b_0 = -Modelica.Mechanics.MultiBody.Frames.resolve1(frame_resolve.R,
              load[4:6]);
            frame_b.f = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_b.R, f_b_0);
            frame_b.t = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_b.R, t_b_0);
            frame_resolve.f = zeros(3);
            frame_resolve.t = zeros(3);
          end if;

          // Force and torque balance
          r_0 = frame_b.r_0 - frame_a.r_0;
          zeros(3) = frame_a.f + Modelica.Mechanics.MultiBody.Frames.resolve2(frame_a.R,
            f_b_0);
          zeros(3) = frame_a.t + Modelica.Mechanics.MultiBody.Frames.resolve2(frame_a.R,
            t_b_0 + cross(r_0, f_b_0));
          annotation (
            obsolete="Based on a packed result signal which is not a good design. Use instead Modelica.Mechanics.MultiBody.Forces.ForceAndTorque",
            preferredView="info",
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Rectangle(
                  extent={{-98,99},{99,-98}},
                  lineColor={255,255,255},
                  fillColor={255,255,255},
                  fillPattern=FillPattern.Solid),
                Text(
                  extent={{-59,55},{72,30}},
                  lineColor={192,192,192},
                  textString="resolve"),
                Text(extent={{-136,-52},{149,-113}}, textString="%name"),
                Polygon(
                  points={{100,21},{84,55},{69,39},{100,21}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(
                  points={{40,100},{40,0}},
                  color={95,95,95},
                  pattern=LinePattern.Dot),
                Polygon(
                  points={{-95,1},{-64,11},{-64,-10},{-95,1}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Polygon(
                  points={{-100,20},{-86,53},{-70,42},{-100,20}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(
                  points={{-60,100},{40,100}},
                  color={95,95,95},
                  pattern=LinePattern.Dot),
                Polygon(
                  points={{94,0},{65,12},{65,-11},{94,0}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid),
                Line(points={{-64,0},{-20,0}}),
                Line(points={{20,0},{65,0}}),
                Line(points={{-79,47},{-70,61},{-59,72},{-45,81},{-32,84},{-20,
                      85}}),
                Line(points={{76,47},{66,60},{55,69},{49,74},{41,80},{31,84},{
                      20,85}})}),
            Documentation(info="<HTML>
<p>
The <b>6</b> signals of the <b>load</b> connector are interpreted
as the x-, y- and z-coordinates of a <b>force</b> and as
the x-, y-, and z-coordinates of a <b>torque</b> acting at the frame
connector to which frame_b of this component is attached. If connector
<b>frame_resolve</b> is <b>not</b> connected, the force and torque coordinates
are with respect to <b>frame_b</b>. If connector
<b>frame_resolve</b> is connected, the force and torque coordinates
are with respect to <b>frame_resolve</b>. In this case the
force and torque in connector frame_resolve are set to zero,
i.e., this connector is solely used to provide the information
of the coordinate system, in which the force/torque coordinates
are defined. The input signals are mapped to the force
and torque in the following way:
</p>
<pre>
   force  = load[1:3]
   torque = load[4:6]
</pre>
<p>
Additionally, a force and torque acts on frame_a in such a way that
the force and torque balance between frame_a and frame_b is fulfilled.
</p>
<p>
An example how to use this model is given in the
following figure:
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Forces/ForceAndTorque1.png\">

<p>
This leads to the following animation (the yellow cylinder
characterizes the line between frame_a and frame_b of the
ForceAndTorque component, i.e., the force and torque acts with
negative sign
also on the opposite side of this cylinder, but for
clarity this is not shown in the animation):
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Forces/ForceAndTorque2.png\">
</html>"));
        end ForceAndTorque;
      end Forces;

      package Interfaces
        "Connectors and partial models for 3-dim. mechanical components"
        partial model PartialCutForceSensor
          "Obsolete model. Use instead instead a model from Modelica.Mechanics.MultiBody.Sensors"

          extends Modelica.Icons.RotationalSensor;
          extends ObsoleteModelica3.Icons.ObsoleteModel;
          Modelica.Mechanics.MultiBody.Interfaces.Frame_a frame_a
            "Coordinate system with one cut-force and cut-torque"                          annotation (Placement(
                transformation(extent={{-116,-16},{-84,16}})));
          Modelica.Mechanics.MultiBody.Interfaces.Frame_b frame_b
            "Coordinate system with one cut-force and cut-torque"                          annotation (Placement(
                transformation(extent={{84,-16},{116,16}})));
          Modelica.Mechanics.MultiBody.Interfaces.Frame_resolve frame_resolve
            "If connected, the output signals are resolved in this frame (cut-force/-torque are set to zero)"
            annotation (Placement(transformation(
                origin={80,-100},
                extent={{-16,-16},{16,16}},
                rotation=270)));

        protected
          outer Modelica.Mechanics.MultiBody.World world;
        equation
          defineBranch(frame_a.R, frame_b.R);
          assert(cardinality(frame_a) > 0,
            "Connector frame_a of cut-force/-torque sensor object is not connected");
          assert(cardinality(frame_b) > 0,
            "Connector frame_b of cut-force/-torque sensor object is not connected");

          // frame_a and frame_b are identical
          frame_a.r_0 = frame_b.r_0;
          frame_a.R = frame_b.R;

          // force and torque balance
          zeros(3) = frame_a.f + frame_b.f;
          zeros(3) = frame_a.t + frame_b.t;

          // deduce cut-force
          if cardinality(frame_resolve) == 1 then
            // frame_resolve is connected
            frame_resolve.f = zeros(3);
            frame_resolve.t = zeros(3);
          else
            // frame_resolve is NOT connected
            frame_resolve.r_0 = zeros(3);
            frame_resolve.R = Modelica.Mechanics.MultiBody.Frames.nullRotation();
          end if;
          annotation (
            obsolete="Model equations depend on cardinality(..) which will become obsolete in the Modelica language. Use instead a model from Modelica.Mechanics.MultiBody.Sensors",
            Documentation(info="<html>
<p>
This is a base class for 3-dim. mechanical components with two frames
and one output port in order to measure the cut-force and/or
cut-torque acting between the two frames and
to provide the measured signals as output for further processing
with the blocks of package Modelica.Blocks.
</p>
</html>"),         Icon(coordinateSystem(
                preserveAspectRatio=true,
                extent={{-100,-100},{100,100}}),
                graphics={
                Line(points={{-70,0},{-101,0}}),
                Line(points={{70,0},{100,0}}),
                Line(points={{-80,-100},{-80,0}}, color={0,0,127}),
                Text(
                  extent={{-132,76},{129,124}},
                  textString="%name",
                  lineColor={0,0,255}),
                Text(
                  extent={{-118,55},{-82,30}},
                  lineColor={128,128,128},
                  textString="a"),
                Text(
                  extent={{83,55},{119,30}},
                  lineColor={128,128,128},
                  textString="b"),
                Text(
                  extent={{-31,-72},{100,-97}},
                  lineColor={192,192,192},
                  textString="resolve"),
                Line(
                  points={{80,0},{80,-100}},
                  color={95,95,95},
                  pattern=LinePattern.Dot)}),
            Diagram(coordinateSystem(
                preserveAspectRatio=true,
                extent={{-100,-100},{100,100}}),
                graphics={
                Line(points={{-70,0},{-100,0}}),
                Line(points={{70,0},{100,0}}),
                Line(points={{-80,-100},{-80,0}}, color={0,0,127}),
                Line(
                  points={{80,0},{80,-100}},
                  color={95,95,95},
                  pattern=LinePattern.Dot)}));
        end PartialCutForceSensor;
      end Interfaces;

      package Joints
        package Internal
          model RevoluteWithLengthConstraint
            "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Joints.Internal.RevoluteWithLengthConstraint"

            import SI = Modelica.SIunits;
            import Cv = Modelica.SIunits.Conversions;
            extends Modelica.Mechanics.MultiBody.Interfaces.PartialTwoFrames;
            extends ObsoleteModelica3.Icons.ObsoleteModel;
            Modelica.Mechanics.Rotational.Interfaces.Flange_a axis
              "1-dim. rotational flange that drives the joint"
              annotation (Placement(transformation(extent={{10,90},{-10,110}})));
            Modelica.Mechanics.Rotational.Interfaces.Flange_b bearing
              "1-dim. rotational flange of the drive bearing"
              annotation (Placement(transformation(extent={{-50,90},{-70,110}})));

            Modelica.Blocks.Interfaces.RealInput position_a[3]
              "Position vector from frame_a to frame_a side of length constraint, resolved in frame_a of revolute joint"
              annotation (Placement(transformation(extent={{-140,-80},{-100,-40}})));
            Modelica.Blocks.Interfaces.RealInput position_b[3]
              "Position vector from frame_b to frame_b side of length constraint, resolved in frame_b of revolute joint"
              annotation (Placement(transformation(extent={{140,-80},{100,-40}})));

            parameter Boolean animation=true
              "= true, if animation shall be enabled";
            parameter SI.Position lengthConstraint=1
              "Fixed length of length constraint";
            parameter Modelica.Mechanics.MultiBody.Types.Axis n={0,0,1}
              "Axis of rotation resolved in frame_a (= same as in frame_b)"
              annotation (Evaluate=true);
            parameter Cv.NonSIunits.Angle_deg phi_offset=0
              "Relative angle offset (angle = phi + from_deg(phi_offset))";
            parameter Cv.NonSIunits.Angle_deg phi_guess=0
              "Select the configuration such that at initial time |phi - from_deg(phi_guess)|is minimal";
            parameter SI.Distance cylinderLength=world.defaultJointLength
              "Length of cylinder representing the joint axis"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
            parameter SI.Distance cylinderDiameter=world.defaultJointWidth
              "Diameter of cylinder representing the joint axis"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
            input Modelica.Mechanics.MultiBody.Types.Color cylinderColor=Modelica.Mechanics.MultiBody.Types.Defaults.JointColor
              "Color of cylinder representing the joint axis"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
            input Modelica.Mechanics.MultiBody.Types.SpecularCoefficient
              specularCoefficient=world.defaultSpecularCoefficient
              "Reflection of ambient light (= 0: light is completely absorbed)"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));

            parameter Boolean axisTorqueBalance=true
              "= true, if torque balance of flange axis with the frame_b connector (axis.tau = -e*frame_b.t) shall be defined. Otherwise this equation has to be provided outside of this joint"
              annotation (Dialog(tab="Advanced"));
            final parameter Boolean positiveBranch(fixed=false)
              "Based on phi_guess, selection of one of the two solutions of the non-linear constraint equation";
            final parameter Real e[3](each final unit="1")=Modelica.Math.Vectors.normalize(              n)
              "Unit vector in direction of rotation axis, resolved in frame_a";

            SI.Angle phi "Rotation angle of revolute joint";
            Modelica.Mechanics.MultiBody.Frames.Orientation R_rel
              "Relative orientation object from frame_a to frame_b";
            SI.Angle angle
              "= phi + from_deg(phi_offset) (relative rotation angle between frame_a and frame_b)";
            SI.Torque tau "= axis.tau (driving torque in the axis)";

          protected
            SI.Position r_a[3]=position_a
              "Position vector from frame_a to frame_a side of length constraint, resolved in frame_a of revolute joint";
            SI.Position r_b[3]=position_b
              "Position vector from frame_b to frame_b side of length constraint, resolved in frame_b of revolute joint";
            Real e_r_a "Projection of r_a on e";
            Real e_r_b "Projection of r_b on e";
            Real A "Coefficient A of equation: A*cos(phi) + B*sin(phi) + C = 0";
            Real B "Coefficient B of equation: A*cos(phi) + B*sin(phi) + C = 0";
            Real C "Coefficient C of equation: A*cos(phi) + B*sin(phi) + C = 0";
            Real k1 "Constant of quadratic equation";
            Real k2 "Constant of quadratic equation";
            Real k1a(start=1);
            Real k1b;
            Real kcos_angle "= k1*cos(angle)";
            Real ksin_angle "= k1*sin(angle)";

            Modelica.Mechanics.MultiBody.Visualizers.Advanced.Shape cylinder(
              shapeType="cylinder",
              color=cylinderColor,
              specularCoefficient=specularCoefficient,
              length=cylinderLength,
              width=cylinderDiameter,
              height=cylinderDiameter,
              lengthDirection=e,
              widthDirection={0,1,0},
              r_shape=-e*(cylinderLength/2),
              r=frame_a.r_0,
              R=frame_a.R) if world.enableAnimation and animation;

            function selectBranch
              "Determine branch which is closest to initial angle=0"

              import Modelica.Math.*;
              input SI.Length L "Length of length constraint";
              input Real e[3](each final unit="1")
                "Unit vector along axis of rotation, resolved in frame_a (= same in frame_b)";
              input SI.Angle angle_guess
                "Select the configuration such that at initial time |angle-angle_guess|is minimal (angle=0: frame_a and frame_b coincide)";
              input SI.Position r_a[3]
                "Position vector from frame_a to frame_a side of length constraint, resolved in frame_a of revolute joint";
              input SI.Position r_b[3]
                "Position vector from frame_b to frame_b side of length constraint, resolved in frame_b of revolute joint";
              output Boolean positiveBranch "Branch of the initial solution";
            protected
              Real e_r_a "Projection of r_a on e";
              Real e_r_b "Projection of r_b on e";
              Real A
                "Coefficient A of equation: A*cos(phi) + B*sin(phi) + C = 0";
              Real B
                "Coefficient B of equation: A*cos(phi) + B*sin(phi) + C = 0";
              Real C
                "Coefficient C of equation: A*cos(phi) + B*sin(phi) + C = 0";
              Real k1 "Constant of quadratic equation";
              Real k2 "Constant of quadratic equation";
              Real kcos1 "k1*cos(angle1)";
              Real ksin1 "k1*sin(angle1)";
              Real kcos2 "k2*cos(angle2)";
              Real ksin2 "k2*sin(angle2)";
              SI.Angle angle1 "solution 1 of nonlinear equation";
              SI.Angle angle2 "solution 2 of nonlinear equation";
            algorithm
              /* The position vector r_rel from frame_a to frame_b of the length constraint
       element, resolved in frame_b of the revolute joint is given by
       (T_rel is the planar transformation matrix from frame_a to frame_b of
        the revolute joint):
          r_rel = r_b - T_rel*r_a
       The length constraint can therefore be formulated as:
          r_rel*r_rel = L*L
       with
          (r_b - T_rel*r_a)*(r_b - T_rel*r_a)
             = r_b*r_b - 2*r_b*T_rel*r_a + r_a*transpose(T_rel)*T_rel*r_a
             = r_b*r_b + r_a*r_a - 2*r_b*T_rel*r_a
       follows
          (1) 0 = r_a*r_a + r_b*r_b - 2*r_b*T_rel*r_a - L*L
       The vectors r_a, r_b and parameter L are NOT a function of
       the angle of the revolute joint. Since T_rel = T_rel(angle) is a function
       of the unknown angle of the revolute joint, this is a non-linear
       equation in this angle.
          T_rel = [e]*transpose([e]) + (identity(3) - [e]*transpose([e]))*cos(angle)
                  - skew(e)*sin(angle);
       with
          r_b*T_rel*r_a
             = r_b*(e*(e*r_a) + (r_a - e*(e*r_a))*cos(angle) - cross(e,r_a)*sin(angle)
             = (e*r_b)*(e*r_a) + (r_b*r_a - (e*r_b)*(e*r_a))*cos(angle) - r_b*cross(e,r_a)*sin(angle)
       follows for the constraint equation (1)
          (2) 0 = r_a*r_a + r_b*r_b - L*L
                  - 2*(e*r_b)*(e*r_a)
                  - 2*(r_b*r_a - (e*r_b)*(e*r_a))*cos(angle)
                  + 2*r_b*cross(e,r_a)*sin(angle)
       or
          (3) A*cos(angle) + B*sin(angle) + C = 0
       with
              A = -2*(r_b*r_a - (e*r_b)*(e*r_a))
              B = 2*r_b*cross(e,r_a)
              C = r_a*r_a + r_b*r_b - L*L - 2*(e*r_b)*(e*r_a)
       Equation (3) is solved by computing sin(angle) and cos(angle)
       independently from each other. This allows to compute
       angle in the range: -180 deg <= angle <= 180 deg
    */
              e_r_a := e*r_a;
              e_r_b := e*r_b;
              A := -2*(r_b*r_a - e_r_b*e_r_a);
              B := 2*r_b*cross(e, r_a);
              C := r_a*r_a + r_b*r_b - L*L - 2*e_r_b*e_r_a;
              k1 := A*A + B*B;
              k2 := sqrt(k1 - C*C);

              kcos1 := -A*C + B*k2;
              ksin1 := -B*C - A*k2;
              angle1 := atan2(ksin1, kcos1);

              kcos2 := -A*C - B*k2;
              ksin2 := -B*C + A*k2;
              angle2 := atan2(ksin2, kcos2);

              if abs(angle1 - angle_guess) <= abs(angle2 - angle_guess) then
                positiveBranch := true;
              else
                positiveBranch := false;
              end if;
            end selectBranch;
          initial equation
            positiveBranch = selectBranch(lengthConstraint, e, Cv.from_deg(phi_offset
               + phi_guess), r_a, r_b);
          equation
            Connections.branch(frame_a.R, frame_b.R);
            axis.tau = tau;
            axis.phi = phi;
            bearing.phi = 0;

            angle = Cv.from_deg(phi_offset) + phi;

            // transform kinematic quantities from frame_a to frame_b
            frame_b.r_0 = frame_a.r_0;

            R_rel = Modelica.Mechanics.MultiBody.Frames.planarRotation(
              e,
              angle,
              der(angle));
            frame_b.R = Modelica.Mechanics.MultiBody.Frames.absoluteRotation(frame_a.R,
              R_rel);

            // Transform the force and torque acting at frame_b to frame_a
            zeros(3) = frame_a.f + Modelica.Mechanics.MultiBody.Frames.resolve1(R_rel,
              frame_b.f);
            zeros(3) = frame_a.t + Modelica.Mechanics.MultiBody.Frames.resolve1(R_rel,
              frame_b.t);

            if axisTorqueBalance then
              /* Note, if axisTorqueBalance is false, the force in the
       length constraint must be calculated such that the driving
       Torque in direction of the rotation axis is:
          axis.tau = -e*frame_b.t;
       If axisTorqueBalance=true, this equation is provided here.
       As a consequence, the force in the length constraint and the second
       derivative of 'angle' will be part of a linear algebraic system of
       equations (otherwise, it might be possible to remove this force
       from the linear system).
    */
              tau = -e*frame_b.t;
            end if;

            // Compute rotation angle (details, see function "selectBranch")
            e_r_a = e*r_a;
            e_r_b = e*r_b;
            A = -2*(r_b*r_a - e_r_b*e_r_a);
            B = 2*r_b*cross(e, r_a);
            C = r_a*r_a + r_b*r_b - lengthConstraint*lengthConstraint - 2*e_r_b*e_r_a;
            k1 = A*A + B*B;
            k1a = k1 - C*C;

            assert(k1a > 1.e-10, "
Singular position of loop (either no or two analytic solutions;
the mechanism has lost one-degree-of freedom in this position).
Try first to use another Modelica.Mechanics.MultiBody.Joints.Assemblies.JointXXX component.
In most cases it is best that the joints outside of the JointXXX
component are revolute and NOT prismatic joints. If this also
lead to singular positions, it could be that this kinematic loop
cannot be solved analytically. In this case you have to build
up the loop with basic joints (NO aggregation JointXXX components)
and rely on dynamic state selection, i.e., during simulation
the states will be dynamically selected in such a way that in no
position a degree of freedom is lost.
");

            k1b = Modelica.Mechanics.MultiBody.Frames.Internal.maxWithoutEvent(k1a,
              1.0e-12);
            k2 = sqrt(k1b);
            kcos_angle = -A*C + (if positiveBranch then B else -B)*k2;
            ksin_angle = -B*C + (if positiveBranch then -A else A)*k2;

            angle = Modelica.Math.atan2(ksin_angle, kcos_angle);
            annotation (
              defaultConnectionStructurallyInconsistent=true,
              preferredView="info",
              obsolete="Obsolete model that is not balanced. Use instead Modelica.Mechanics.MultiBody.Joints.Internal.RevoluteWithLengthConstraint",
              Icon(coordinateSystem(
                  preserveAspectRatio=false,
                  extent={{-100,-100},{100,100}}),
                  graphics={
                  Rectangle(
                    extent={{-30,10},{10,-10}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-100,-60},{-30,60}},
                    lineColor={0,0,0},
                    fillPattern=FillPattern.HorizontalCylinder,
                    fillColor={192,192,192}),
                  Rectangle(
                    extent={{30,-60},{100,60}},
                    lineColor={0,0,0},
                    fillPattern=FillPattern.HorizontalCylinder,
                    fillColor={192,192,192}),
                  Text(extent={{-139,-168},{137,-111}}, textString="%name"),
                  Rectangle(extent={{-100,60},{-30,-60}}, lineColor={0,0,0}),
                  Rectangle(extent={{30,60},{100,-60}}, lineColor={0,0,0}),
                  Text(
                    extent={{-142,-108},{147,-69}},
                    lineColor={0,0,0},
                    textString="n=%n"),
                  Line(points={{-60,60},{-60,90}}),
                  Line(points={{-20,70},{-60,70}}),
                  Line(points={{-20,80},{-20,60}}),
                  Line(points={{20,80},{20,60}}),
                  Line(points={{20,70},{41,70}}),
                  Polygon(
                    points={{-9,30},{10,30},{30,50},{-29,50},{-9,30}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Polygon(
                    points={{10,30},{30,50},{30,-51},{10,-31},{10,30}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-10,90},{10,50}},
                    lineColor={0,0,0},
                    fillPattern=FillPattern.VerticalCylinder,
                    fillColor={192,192,192})}),
              Diagram(coordinateSystem(
                  preserveAspectRatio=false,
                  extent={{-100,-100},{100,100}}),
                  graphics={
                  Rectangle(
                    extent={{-100,-60},{-30,60}},
                    lineColor={0,0,0},
                    fillPattern=FillPattern.HorizontalCylinder,
                    fillColor={192,192,192}),
                  Rectangle(
                    extent={{-30,10},{10,-10}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{30,-60},{100,60}},
                    lineColor={0,0,0},
                    fillPattern=FillPattern.HorizontalCylinder,
                    fillColor={192,192,192}),
                  Line(points={{-60,60},{-60,96}}),
                  Line(points={{-20,70},{-60,70}}),
                  Line(points={{-20,80},{-20,60}}),
                  Line(points={{20,80},{20,60}}),
                  Line(points={{20,70},{41,70}}),
                  Polygon(
                    points={{-9,30},{10,30},{30,50},{-29,50},{-9,30}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Polygon(
                    points={{10,30},{30,50},{30,-51},{10,-31},{10,30}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-10,50},{10,100}},
                    lineColor={0,0,0},
                    fillPattern=FillPattern.VerticalCylinder,
                    fillColor={192,192,192})}),
              Documentation(info="<HTML>
<p>
Joint where frame_b rotates around axis n which is fixed in frame_a.
The two frames coincide when \"phi + phi_offset = 0\", where
\"phi_offset\" is a parameter with a zero default
and \"phi\" is the rotation angle.
</p>
<p>
This variant of the revolute joint is designed to work together
with a length constraint in a kinematic loop. This means that the
angle of the revolute joint, phi, is computed such that the
length constraint is fulfilled.
</p>
<p>
<b>Usually, this joint should not be used by a user of the MultiBody
library. It is only provided to built-up the Modelica.Mechanics.MultiBody.Joints.Assemblies.JointXYZ
joints.</b>
</p>
</html>"));
          end RevoluteWithLengthConstraint;

          model PrismaticWithLengthConstraint
            "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Joints.Internal.PrismaticWithLengthConstraint"

            import SI = Modelica.SIunits;
            import Cv = Modelica.SIunits.Conversions;
            extends Modelica.Mechanics.MultiBody.Interfaces.PartialTwoFrames;
            extends ObsoleteModelica3.Icons.ObsoleteModel;
            Modelica.Mechanics.Translational.Interfaces.Flange_a axis
              "1-dim. translational flange that drives the joint"
              annotation (Placement(transformation(extent={{70,80},{90,60}})));
            Modelica.Mechanics.Translational.Interfaces.Flange_b bearing
              "1-dim. translational flange of the drive bearing"
              annotation (Placement(transformation(extent={{-30,80},{-50,60}})));
            Modelica.Blocks.Interfaces.RealInput position_a[3]
              "Position vector from frame_a to frame_a side of length constraint, resolved in frame_a of revolute joint"
              annotation (Placement(transformation(extent={{-140,-80},{-100,-40}})));
            Modelica.Blocks.Interfaces.RealInput position_b[3]
              "Position vector from frame_b to frame_b side of length constraint, resolved in frame_b of revolute joint"
              annotation (Placement(transformation(extent={{140,-80},{100,-40}})));

            parameter Boolean animation=true
              "= true, if animation shall be enabled";
            parameter SI.Position length=1 "Fixed length of length constraint";
            parameter Modelica.Mechanics.MultiBody.Types.Axis n={1,0,0}
              "Axis of translation resolved in frame_a (= same as in frame_b)"
              annotation (Evaluate=true);
            parameter SI.Position s_offset=0
              "Relative distance offset (distance between frame_a and frame_b = s(t) + s_offset)";
            parameter SI.Position s_guess=0
              "Select the configuration such that at initial time |s(t0)-s_guess|is minimal";
            parameter Modelica.Mechanics.MultiBody.Types.Axis boxWidthDirection={0,1,0}
              "Vector in width direction of box, resolved in frame_a"
              annotation (Evaluate=true, Dialog(tab="Animation", group=
                    "if animation = true", enable=animation));
            parameter SI.Distance boxWidth=world.defaultJointWidth
              "Width of prismatic joint box"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
            parameter SI.Distance boxHeight=boxWidth
              "Height of prismatic joint box"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
            input Modelica.Mechanics.MultiBody.Types.Color boxColor=Modelica.Mechanics.MultiBody.Types.Defaults.JointColor
              "Color of prismatic joint box"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
            input Modelica.Mechanics.MultiBody.Types.SpecularCoefficient
              specularCoefficient=world.defaultSpecularCoefficient
              "Reflection of ambient light (= 0: light is completely absorbed)"
              annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));

            parameter Boolean axisForceBalance=true
              "= true, if force balance of flange axis with the frame_b connector (axis.f = -e*frame_b.f) shall be defined. Otherwise this equation has to be provided outside of this joint"
              annotation (Dialog(tab="Advanced"));
            final parameter Boolean positiveBranch(fixed=false)
              "Selection of one of the two solutions of the non-linear constraint equation";
            final parameter Real e[3](each final unit="1")=Modelica.Math.Vectors.normalize(              n)
              "Unit vector in direction of translation axis, resolved in frame_a";
            SI.Position s
              "Relative distance between frame_a and frame_b along axis n = s + s_offset)";
            SI.Position distance
              "Relative distance between frame_a and frame_b along axis n";
            SI.Position r_rel_a[3]
              "Position vector from frame_a to frame_b resolved in frame_a";
            SI.Force f "= axis.f (driving force in the axis)";

          protected
            SI.Position r_a[3]=position_a
              "Position vector from frame_a to frame_a side of length constraint, resolved in frame_a of revolute joint";
            SI.Position r_b[3]=position_b
              "Position vector from frame_b to frame_b side of length constraint, resolved in frame_b of revolute joint";
            Modelica.SIunits.Position rbra[3] "= rb - ra";
            Real B "Coefficient B of equation: s*s + B*s + C = 0";
            Real C "Coefficient C of equation: s*s + B*s + C = 0";
            Real k1 "Constant of quadratic equation solution";
            Real k2 "Constant of quadratic equation solution";
            Real k1a(start=1);
            Real k1b;

            Modelica.Mechanics.MultiBody.Visualizers.Advanced.Shape box(
              shapeType="box",
              color=boxColor,
              specularCoefficient=specularCoefficient,
              length=if noEvent(abs(s + s_offset) > 1.e-6) then s + s_offset else 1.e-6,
              width=boxWidth,
              height=boxHeight,
              lengthDirection=e,
              widthDirection=boxWidthDirection,
              r=frame_a.r_0,
              R=frame_a.R) if world.enableAnimation and animation;

            function selectBranch
              "Determine branch which is closest to initial angle=0"
              import Modelica.Math.*;
              input SI.Length L "Length of length constraint";
              input Real e[3](each final unit="1")
                "Unit vector along axis of translation, resolved in frame_a (= same in frame_b)";
              input SI.Position d_guess
                "Select the configuration such that at initial time |d-d_guess|is minimal (d: distance between origin of frame_a and origin of frame_b)";
              input SI.Position r_a[3]
                "Position vector from frame_a to frame_a side of length constraint, resolved in frame_a of prismatic joint";
              input SI.Position r_b[3]
                "Position vector from frame_b to frame_b side of length constraint, resolved in frame_b of prismatic joint";
              output Boolean positiveBranch "Branch of the initial solution";
            protected
              Modelica.SIunits.Position rbra[3] "= rb - ra";
              Real B "Coefficient B of equation: d*d + B*d + C = 0";
              Real C "Coefficient C of equation: d*d + B*d + C = 0";
              Real k1 "Constant of quadratic equation solution";
              Real k2 "Constant of quadratic equation solution";
              Real d1 "solution 1 of quadratic equation";
              Real d2 "solution 2 of quadratic equation";
            algorithm
              /* The position vector r_rel from frame_a to frame_b of the length constraint
       element, resolved in frame_b of the prismatic joint (frame_a and frame_b
       of the prismatic joint are parallel to each other) is given by:
          r_rel = d*e + r_b - r_a
       The length constraint can therefore be formulated as:
          r_rel*r_rel = L*L
       with
          (d*e + r_b - r_a)*(d*e + r_b - r_a)
                   = d*d + 2*d*e*(r_b - r_a) + (r_b - r_a)*(r_b - r_a)
       follows
          (1)  0 = d*d + d*2*e*(r_b - r_a) + (r_b - r_a)*(r_b - r_a) - L*L
       The vectors r_a, r_b and parameter L are NOT a function of
       the distance d of the prismatic joint. Therefore, (1) is a quadratic
       equation in the single unknown "d":
          (2) d*d + B*d + C = 0
              with   B = 2*e*(r_b - r_a)
                     C = (r_b - r_a)*(r_b - r_a) - L*L
       The solution is
          (3) d = - B/2 +/- sqrt(B*B/4 - C)
    */
              rbra := r_b - r_a;
              B := 2*(e*rbra);
              C := rbra*rbra - L*L;
              k1 := B/2;
              k2 := sqrt(k1*k1 - C);
              d1 := -k1 + k2;
              d2 := -k1 - k2;
              if abs(d1 - d_guess) <= abs(d2 - d_guess) then
                positiveBranch := true;
              else
                positiveBranch := false;
              end if;
            end selectBranch;
          initial equation
            positiveBranch = selectBranch(length, e, s_offset + s_guess, r_a, r_b);
          equation
            Connections.branch(frame_a.R, frame_b.R);
            axis.f = f;
            axis.s = s;
            bearing.s = 0;
            distance = s_offset + s;

            // relationships of frame_a and frame_b quantities
            r_rel_a = e*distance;
            frame_b.r_0 = frame_a.r_0 + Modelica.Mechanics.MultiBody.Frames.resolve1(
              frame_a.R, r_rel_a);
            frame_b.R = frame_a.R;
            zeros(3) = frame_a.f + frame_b.f;
            zeros(3) = frame_a.t + frame_b.t + cross(r_rel_a, frame_b.f);

            if axisForceBalance then
              /* Note, if axisForceBalance is false, the force in the
       length constraint must be calculated such that the driving
       force in direction of the translation axis is:
          axis.f = -e*frame_b.f;
       If axisForceBalance=true, this equation is provided here.
       As a consequence, the force in the length constraint will be
       part of a linear algebraic system of equations (otherwise, it
       might be possible to remove this force from the linear system).
    */
              f = -e*frame_b.f;
            end if;

            // Compute translational distance (details, see function "selectBranch")
            rbra = r_b - r_a;
            B = 2*(e*rbra);
            C = rbra*rbra - length*length;
            k1 = B/2;
            k1a = k1*k1 - C;
            assert(noEvent(k1a > 1.e-10), "
Singular position of loop (either no or two analytic solutions;
the mechanism has lost one-degree-of freedom in this position).
Try first to use another Modelica.Mechanics.MultiBody.Joints.Assemblies.JointXXX component.
If this also lead to singular positions, it could be that this
kinematic loop cannot be solved analytically with a fixed state
selection. In this case you have to build up the loop with
basic joints (NO aggregation JointXXX components) and rely on
dynamic state selection, i.e., during simulation the states will
be dynamically selected in such a way that in no position a
degree of freedom is lost.
");
            k1b = Modelica.Mechanics.MultiBody.Frames.Internal.maxWithoutEvent(k1a,
            1.0e-12);
            k2 = sqrt(k1b);
            distance = -k1 + (if positiveBranch then k2 else -k2);
            annotation (
              defaultConnectionStructurallyInconsistent=true,
              preferredView="info",
              obsolete="Obsolete model that is not balanced. Use instead Modelica.Mechanics.MultiBody.Joints.Internal.PrismaticWithLengthConstraint",
              Icon(coordinateSystem(
                  preserveAspectRatio=false,
                  extent={{-100,-100},{100,100}}),
                  graphics={
                  Rectangle(
                    extent={{-30,-40},{100,30}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(extent={{-30,40},{100,-40}}, lineColor={0,0,0}),
                  Rectangle(
                    extent={{-100,-60},{-30,50}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-100,50},{-30,60}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={0,0,0},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-30,30},{100,40}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={0,0,0},
                    fillPattern=FillPattern.Solid),
                  Text(extent={{-136,-170},{140,-113}}, textString="%name"),
                  Rectangle(extent={{-100,60},{-30,-60}}, lineColor={0,0,0}),
                  Line(points={{100,-40},{100,-60}}),
                  Rectangle(
                    extent={{100,40},{90,80}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Text(
                    extent={{-136,-116},{153,-77}},
                    lineColor={0,0,0},
                    textString="n=%n")}),
              Diagram(coordinateSystem(
                  preserveAspectRatio=false,
                  extent={{-100,-100},{100,100}}),
                  graphics={
                  Line(points={{-30,-50},{-30,50}}),
                  Line(points={{0,-67},{90,-67}}, color={128,128,128}),
                  Text(
                    extent={{31,-68},{68,-81}},
                    lineColor={128,128,128},
                    textString="s"),
                  Line(points={{-100,-67},{0,-67}}, color={128,128,128}),
                  Polygon(
                    points={{-39,-64},{-29,-67},{-39,-70},{-39,-64}},
                    lineColor={128,128,128},
                    fillColor={128,128,128},
                    fillPattern=FillPattern.Solid),
                  Text(
                    extent={{-77,-70},{-43,-85}},
                    lineColor={128,128,128},
                    textString="s_offset"),
                  Line(points={{-100,-71},{-100,-51}}, color={128,128,128}),
                  Line(points={{-30,-73},{-30,-33}}, color={128,128,128}),
                  Line(points={{100,-70},{100,-30}}, color={128,128,128}),
                  Polygon(
                    points={{90,-64},{100,-67},{90,-70},{90,-64}},
                    lineColor={128,128,128},
                    fillColor={128,128,128},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-100,50},{-30,60}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={0,0,0},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-100,-60},{-30,50}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(extent={{-30,40},{100,-40}}, lineColor={0,0,0}),
                  Rectangle(
                    extent={{-30,-40},{100,30}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid),
                  Rectangle(
                    extent={{-30,30},{100,40}},
                    lineColor={0,0,255},
                    pattern=LinePattern.None,
                    fillColor={0,0,0},
                    fillPattern=FillPattern.Solid),
                  Rectangle(extent={{-100,60},{-30,-60}}, lineColor={0,0,0}),
                  Line(points={{100,-40},{100,-60}}),
                  Text(extent={{42,91},{57,76}}, textString="f"),
                  Line(points={{40,75},{70,75}}, color={0,0,255}),
                  Polygon(
                    points={{-21,78},{-31,75},{-21,72},{-21,78}},
                    lineColor={0,0,255},
                    fillColor={0,0,255},
                    fillPattern=FillPattern.Solid),
                  Line(points={{-8,75},{-31,75}}, color={0,0,255}),
                  Text(extent={{-21,90},{-6,75}}, textString="f"),
                  Polygon(
                    points={{60,78},{70,75},{60,72},{60,78}},
                    lineColor={0,0,255},
                    fillColor={0,0,255},
                    fillPattern=FillPattern.Solid),
                  Line(points={{-30,64},{70,64}}, color={128,128,128}),
                  Polygon(
                    points={{60,67},{70,64},{60,61},{60,67}},
                    lineColor={128,128,128},
                    fillColor={128,128,128},
                    fillPattern=FillPattern.Solid),
                  Text(
                    extent={{0,63},{37,50}},
                    lineColor={128,128,128},
                    textString="s"),
                  Rectangle(
                    extent={{100,40},{90,80}},
                    lineColor={0,0,0},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid)}),
              Documentation(info="<HTML>
<p>
Joint where frame_b is translated along axis n which is fixed in frame_a.
The two frames coincide when \"s + s_offset = 0\", where
\"s_offset\" is a parameter with a zero default
and \"s\" is the relative distance.
</p>
<p>
This variant of the prismatic joint is designed to work together
with a length constraint in a kinematic loop. This means that the
relative distance \"s\" of the joint is computed such that the
length constraint is fulfilled.
</p>
<p>
<b>Usually, this joint should not be used by a user of the MultiBody
library. It is only provided to built-up the Modelica.Mechanics.MultiBody.Joints.Assemblies.JointXYZ
joints.</b>
</p>
</html>"));
          end PrismaticWithLengthConstraint;
        end Internal;
      end Joints;

      package Sensors "Sensors to measure variables"
        model AbsoluteSensor
          "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Sensors.AbsoluteSensor"
          import SI = Modelica.SIunits;
          import Modelica.Mechanics.MultiBody.Frames;
          import Modelica.Mechanics.MultiBody.Types;
          extends Modelica.Mechanics.MultiBody.Interfaces.PartialAbsoluteSensor(final
              n_out=3*((if get_r_abs then 1 else 0) + (if get_v_abs then 1 else 0) + (
                if get_a_abs then 1 else 0) + (if get_angles then 1 else 0) + (if
                get_w_abs then 1 else 0) + (if get_z_abs then 1 else 0)));
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.Mechanics.MultiBody.Interfaces.Frame_resolve frame_resolve
            "If connected, the output signals are resolved in this frame"
            annotation (Placement(transformation(
                origin={0,100},
                extent={{-16,-16},{16,16}},
                rotation=270)));
          parameter Boolean animation=true
            "= true, if animation shall be enabled (show arrow)";
          parameter Boolean resolveInFrame_a=false
            "= true, if vectors are resolved in frame_a, otherwise in the world frame (if connector frame_resolve is connected, vectors are resolved in frame_resolve)";
          parameter Boolean get_r_abs=true
            "= true, to measure the position vector from the origin of the world frame to the origin of frame_a in [m]";
          parameter Boolean get_v_abs=false
            "= true, to measure the absolute velocity of the origin of frame_a in [m/s]";
          parameter Boolean get_a_abs=false
            "= true, to measure the absolute acceleration of the origin of frame_a in [m/s^2]";
          parameter Boolean get_angles=false
            "= true, to measure the 3 rotation angles to rotate the world frame into frame_a along the axes defined in 'sequence' below in [rad]";
          parameter Boolean get_w_abs=false
            "= true, to measure the absolute angular velocity of frame_a in [rad/s]";
          parameter Boolean get_z_abs=false
            "= true, to measure the absolute angular acceleration to frame_a in [rad/s^2]";
          parameter Types.RotationSequence sequence(
            min={1,1,1},
            max={3,3,3}) = {1,2,3}
            " Angles are returned to rotate world frame around axes sequence[1], sequence[2] and finally sequence[3] into frame_a"
            annotation (Evaluate=true, Dialog(group="if get_angles = true", enable=get_angles));
          parameter SI.Angle guessAngle1=0
            " Select angles[1] such that abs(angles[1] - guessAngle1) is a minimum"
            annotation (Dialog(group="if get_angles = true", enable=get_angles));
          input SI.Diameter arrowDiameter=world.defaultArrowDiameter
            " Diameter of arrow from world frame to frame_a"
            annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
          input Types.Color arrowColor=Modelica.Mechanics.MultiBody.Types.Defaults.SensorColor
            " Color of arrow from world frame to frame_a"
            annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
          input Types.SpecularCoefficient specularCoefficient = world.defaultSpecularCoefficient
            "Reflection of ambient light (= 0: light is completely absorbed)"
            annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));

        protected
          SI.Position r_abs[3]
            "Dummy or position vector from origin of the world frame to origin of frame_a (resolved in frame_resolve, frame_a or world frame)";
          SI.Velocity v_abs[3]
            "Dummy or velocity of origin of frame_a with respect to origin of world frame (resolved in frame_resolve, frame_a or world frame)";
          SI.Acceleration a_abs[3]
            "Dummy or acceleration of origin of frame_a with respect to origin of word frame (resolved in frame_resolve, frame_a or world frame)";
          SI.Angle angles[3]
            "Dummy or angles to rotate world frame into frame_a via 'sequence'";
          SI.AngularVelocity w_abs[3]
            "Dummy or angular velocity of frame_a with respect to world frame (resolved in frame_resolve, frame_a or world frame)";
          SI.AngularAcceleration z_abs[3]
            "Dummy or angular acceleration of frame_a with respect to world frame (resolved in frame_resolve, frame_a or world frame)";

          SI.Velocity v_abs_0[3]
            "Dummy or absolute velocity of origin of frame_a resolved in world frame";
          SI.AngularVelocity w_abs_0[3]
            "Dummy or absolute angular velocity of frame_a resolved in world frame";
          parameter Integer i1=1;
          parameter Integer i2=if get_r_abs then i1 + 3 else i1;
          parameter Integer i3=if get_v_abs then i2 + 3 else i2;
          parameter Integer i4=if get_a_abs then i3 + 3 else i3;
          parameter Integer i5=if get_angles then i4 + 3 else i4;
          parameter Integer i6=if get_w_abs then i5 + 3 else i5;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Arrow arrow(
            r_head=frame_a.r_0,
            diameter=arrowDiameter,
            specularCoefficient=specularCoefficient,
            color=arrowColor) if world.enableAnimation and animation;
        equation
          if get_angles then
            angles = Frames.axesRotationsAngles(frame_a.R, sequence, guessAngle1);
          else
            angles = zeros(3);
          end if;

          if cardinality(frame_resolve) == 1 then
            // frame_resolve is connected
            frame_resolve.f = zeros(3);
            frame_resolve.t = zeros(3);

            if get_r_abs then
              r_abs = Frames.resolve2(frame_resolve.R, frame_a.r_0);
            else
              r_abs = zeros(3);
            end if;

            if get_v_abs or get_a_abs then
              v_abs_0 = der(frame_a.r_0);
              v_abs = Frames.resolve2(frame_resolve.R, v_abs_0);
            else
              v_abs_0 = zeros(3);
              v_abs = zeros(3);
            end if;

            if get_a_abs then
              a_abs = Frames.resolve2(frame_resolve.R, der(v_abs_0));
            else
              a_abs = zeros(3);
            end if;

            if get_w_abs or get_z_abs then
              w_abs_0 = Modelica.Mechanics.MultiBody.Frames.angularVelocity1(frame_a.R);
              w_abs = Frames.resolve2(frame_resolve.R, w_abs_0);
            else
              w_abs_0 = zeros(3);
              w_abs = zeros(3);
            end if;

            if get_z_abs then
              z_abs = Frames.resolve2(frame_resolve.R, der(w_abs_0));
            else
              z_abs = zeros(3);
            end if;
          else
            // frame_resolve is NOT connected
            frame_resolve.r_0 = zeros(3);
            frame_resolve.R = Frames.nullRotation();

            if get_r_abs then
              if resolveInFrame_a then
                r_abs = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_a.R, frame_a.r_0);
              else
                r_abs = frame_a.r_0;
              end if;
            else
              r_abs = zeros(3);
            end if;

            if get_v_abs or get_a_abs then
              v_abs_0 = der(frame_a.r_0);
              if resolveInFrame_a then
                v_abs = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_a.R, v_abs_0);
              else
                v_abs = v_abs_0;
              end if;
            else
              v_abs_0 = zeros(3);
              v_abs = zeros(3);
            end if;

            if get_a_abs then
              if resolveInFrame_a then
                a_abs = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_a.R, der(v_abs_0));
              else
                a_abs = der(v_abs_0);
              end if;
            else
              a_abs = zeros(3);
            end if;

            w_abs_0 = zeros(3);
            if get_w_abs or get_z_abs then
              if resolveInFrame_a then
                w_abs = Modelica.Mechanics.MultiBody.Frames.angularVelocity2(frame_a.R);
              else
                w_abs = Modelica.Mechanics.MultiBody.Frames.angularVelocity1(frame_a.R);
              end if;
            else
              w_abs = zeros(3);
            end if;

            if get_z_abs then
              /* if w_abs and z_abs are resolved in the world frame, we have
            z_abs = der(w_abs)
         if w_abs and z_abs are resolved in frame_a, we have
            z_abs = R*der(transpose(R)*w_abs)
                  = R*(der(transpose(R))*w_abs + transpose(R)*der(w_abs)))
                  = R*(transpose(R)*R*der(transpose(R))*w_abs + transpose(R)*der(w_abs)))
                  = skew(w_abs)*w_abs + der(w_abs)
                  = der(w_abs)  // since cross(w_abs, w_abs) = 0
      */
              z_abs = der(w_abs);
            else
              z_abs = zeros(3);
            end if;
          end if;

          frame_a.f = zeros(3);
          frame_a.t = zeros(3);

          if get_r_abs then
            y[i1:i1 + 2] = r_abs;
          end if;

          if get_v_abs then
            y[i2:i2 + 2] = v_abs;
          end if;

          if get_a_abs then
            y[i3:i3 + 2] = a_abs;
          end if;

          if get_angles then
            y[i4:i4 + 2] = angles;
          end if;

          if get_w_abs then
            y[i5:i5 + 2] = w_abs;
          end if;

          if get_z_abs then
            y[i6:i6 + 2] = z_abs;
          end if;
          annotation (
            obsolete="Based on a packed result signal which is not a good design. Use instead Modelica.Mechanics.MultiBody.Sensors.AbsoluteSensor",
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Text(
                  extent={{19,109},{150,84}},
                  lineColor={192,192,192},
                  textString="resolve"),
                Line(
                  points={{-84,0},{-84,84},{0,84},{0,100}},
                  color={95,95,95},
                  pattern=LinePattern.Dot),
                Text(
                  extent={{-132,52},{-96,27}},
                  lineColor={128,128,128},
                  textString="a")}),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics={Line(
                  points={{-84,0},{-84,82},{0,82},{0,98}},
                  color={95,95,95},
                  pattern=LinePattern.Dot)}),
            Documentation(info="<HTML>
<p>
Absolute kinematic quantities of frame_a are
computed and provided at the output signal connector <b>y</b>
in packed format in the order
</p>
<ol>
<li> absolute position vector (= r_abs)</li>
<li> absolute velocity vector (= v_abs)</li>
<li> absolute acceleration vector (= a_abs)</li>
<li> 3 angles to rotate the world frame into frame_a (= angles)</li>
<li> absolute angular velocity vector (= w_abs)</li>
<li> absolute angular acceleration vector (= z_abs)</li>
</ol>
<p>
For example, if parameters <b>get_v</b> and <b>get_w</b>
are <b>true</b> and all other get_XXX parameters are <b>false</b>, then
y contains 6 elements:
</p>
<pre>
 y[1:3] = absolute velocity
 y[4:6] = absolute angular velocity
</pre>
<p>
In the following figure the animation of an AbsoluteSensor
component is shown. The light blue coordinate system is
frame_a and the yellow arrow is the animated sensor.
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Sensors/AbsoluteSensor.png\">

<p>
If <b>frame_resolve</b> is connected to another frame, then the
provided absolute kinematic vectors are resolved in this frame.
If <b>frame_resolve</b> is <b>not</b> connected then the
coordinate system in which the relative quantities are
resolved is defined by parameter <b>resolveInFrame_a</b>.
If this parameter is <b>true</b>, then the
provided kinematic vectors are resolved in frame_a of this
component. Otherwise, the kinematic vectors are resolved in
the world frame. For example, if frame_resolve is not
connected and if resolveInFrame_a = <b>false</b>, and
get_v = <b>true</b>, then
</p>
<pre>
  y = <b>der</b>(frame_a.r) // resolved in world frame
</pre>
<p>
is returned, i.e., the derivative of the distance frame_a.r_0
from the origin of the world frame to the origin of frame_a,
resolved in the world frame.
</p>
<p>
Note, the cut-force and the cut-torque in frame_resolve are
always zero, whether frame_resolve is connected or not.
</p>
<p>
If <b>get_angles</b> = <b>true</b>, the 3 angles to rotate the world
frame into frame_a along the axes defined by parameter <b>sequence</b>
are returned. For example, if sequence = {3,1,2} then the world frame is
rotated around angles[1] along the z-axis, afterwards it is rotated
around angles[2] along the x-axis, and finally it is rotated around
angles[3] along the y-axis and is then identical to frame_a.
The 3 angles are returned in the range
</p>
<pre>
    -<font face=\"Symbol\">p</font> &lt;= angles[i] &lt;= <font face=\"Symbol\">p</font>
</pre>
<p>
There are <b>two solutions</b> for \"angles[1]\" in this range.
Via parameter <b>guessAngle1</b> (default = 0) the
returned solution is selected such that |angles[1] - guessAngle1| is
minimal. The transformation matrix between the world frame and
frame_a may be in a singular configuration with respect to \"sequence\", i.e.,
there is an infinite number of angle values leading to the same
transformation matrix. In this case, the returned solution is
selected by setting angles[1] = guessAngle1. Then angles[2]
and angles[3] can be uniquely determined in the above range.
</p>
<p>
Note, that parameter <b>sequence</b> has the restriction that
only values 1,2,3 can be used and that sequence[1] &ne; sequence[2]
and sequence[2] &ne; sequence[3]. Often used values are:
</p>
<pre>
  sequence = <b>{1,2,3}</b>  // Cardan angle sequence
           = <b>{3,1,3}</b>  // Euler angle sequence
           = <b>{3,2,1}</b>  // Tait-Bryan angle sequence
</pre>
<p>
Exact definition of the returned quantities:
</p>
<ol>
<li>r_abs is vector frame_a.r_0, resolved according to table below.</li>
<li>v_abs is vector <b>der</b>(frame_a.r_0), resolved according to table below.</li>
<li>a_abs is vector <b>der</b>(<b>der</b>(frame_a.r_0)), resolved according to
            table below.</li>
<li>angles is a vector of 3 angles such that
    frame_a.R = Frames.axesRotations(sequence, angles).</li>
<li>w_abs is vector Modelica.Mechanics.MultiBody.Frames.angularVelocity1(frame_a.R, <b>der</b>(frame_a.R)),
            resolved according to table below.</li>
<li>z_abs is vector <b>der</b>(w_abs) (= derivative of absolute angular
            velocity of frame_a with respect to the world frame,
            resolved according to table below).</li>
</ol>
<table border=1 cellspacing=0 cellpadding=2>
  <tr><th><b><i>frame_resolve is</i></b></th>
      <th><b><i>resolveInFrame_a =</i></b></th>
      <th><b><i>vector is resolved in</i></b></th>
  </tr>
  <tr><td valign=\"top\">connected</td>
      <td valign=\"top\">true</td>
      <td valign=\"top\"><b>frame_resolve</b></td>
  </tr>
  <tr><td valign=\"top\">connected</td>
      <td valign=\"top\">false</td>
      <td valign=\"top\"><b>frame_resolve</b></td>
  </tr>
  <tr><td valign=\"top\">not connected</td>
      <td valign=\"top\">true</td>
      <td valign=\"top\"><b>frame_a</b></td>
  </tr>
  <tr><td valign=\"top\">not connected</td>
      <td valign=\"top\">false</td>
      <td valign=\"top\"><b>world frame</b></td>
  </tr>
</table><br>
</html>"));
        end AbsoluteSensor;

        model RelativeSensor
          "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Sensors.RelativeSensor"

          import SI = Modelica.SIunits;
          import Modelica.Mechanics.MultiBody.Frames;
          import Modelica.Mechanics.MultiBody.Types;
          extends Modelica.Mechanics.MultiBody.Interfaces.PartialRelativeSensor(final
              n_out=3*((if get_r_rel then 1 else 0) + (if get_v_rel then 1 else 0) + (
                if get_a_rel then 1 else 0) + (if get_angles then 1 else 0) + (if
                get_w_rel then 1 else 0) + (if get_z_rel then 1 else 0)));
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.Mechanics.MultiBody.Interfaces.Frame_resolve frame_resolve
            "If connected, the output signals are resolved in this frame"
            annotation (Placement(transformation(
                origin={-60,-100},
                extent={{-16,-16},{16,16}},
                rotation=270)));

          parameter Boolean animation=true
            "= true, if animation shall be enabled (show arrow)";
          parameter Boolean resolveInFrame_a=true
            "= true, if relative vectors from frame_a to frame_b are resolved before differentiation in frame_a, otherwise in frame_b. If frame_resolve is connected, the vector and its derivatives are resolved in frame_resolve";
          parameter Boolean get_r_rel=true
            "= true, to measure the relative position vector from the origin of frame_a to the origin of frame_b in [m]";
          parameter Boolean get_v_rel=false
            "= true, to measure the relative velocity of the origin of frame_b with respect to frame_a in [m/s]";
          parameter Boolean get_a_rel=false
            "= true, to measure the relative acceleration of the origin of frame_b with respect to frame_a in [m/s^2]";
          parameter Boolean get_angles=false
            "= true, to measure the 3 rotation angles to rotate frame_a into frame_b along the axes defined in 'sequence' below in [rad]";
          parameter Boolean get_w_rel=false
            "= true, to measure the relative angular velocity of frame_b with respect to frame_a in [rad/s]";
          parameter Boolean get_z_rel=false
            "= true, to measure the relative angular acceleration of frame_b with respect to frame_a in [rad/s^2]";
          parameter Types.RotationSequence sequence(
            min={1,1,1},
            max={3,3,3}) = {1,2,3}
            " Angles are returned to rotate frame_a around axes sequence[1], sequence[2] and finally sequence[3] into frame_b"
            annotation (Evaluate=true, Dialog(group="if get_angles = true", enable=get_angles));
          parameter SI.Angle guessAngle1=0
            " Select angles[1] such that abs(angles[1] - guessAngle1) is a minimum"
            annotation (Dialog(group="if get_angles = true", enable=get_angles));
          input SI.Diameter arrowDiameter=world.defaultArrowDiameter
            " Diameter of relative arrow from frame_a to frame_b"
            annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
          input Types.Color arrowColor=Modelica.Mechanics.MultiBody.Types.Defaults.SensorColor
            " Color of relative arrow from frame_a to frame_b"
            annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));
          input Types.SpecularCoefficient specularCoefficient = world.defaultSpecularCoefficient
            "Reflection of ambient light (= 0: light is completely absorbed)"
            annotation (Dialog(tab="Animation", group="if animation = true", enable=animation));

          SI.Position r_rel[3]
            "Dummy or relative position vector (resolved in frame_a, frame_b or frame_resolve)";
          SI.Velocity v_rel[3]
            "Dummy or relative velocity vector (resolved in frame_a, frame_b or frame_resolve";
          SI.Acceleration a_rel[3]
            "Dummy or relative acceleration vector (resolved in frame_a, frame_b or frame_resolve";
          SI.Angle angles[3]
            "Dummy or angles to rotate frame_a into frame_b via 'sequence'";
          SI.AngularVelocity w_rel[3]
            "Dummy or relative angular velocity vector (resolved in frame_a, frame_b or frame_resolve";
          SI.AngularAcceleration z_rel[3]
            "Dummy or relative angular acceleration vector (resolved in frame_a, frame_b or frame_resolve";
          Frames.Orientation R_rel
            "Dummy or relative orientation object from frame_a to frame_b";
        protected
          SI.Position r_rel_ab[3]
            "Dummy or relative position vector resolved in frame_a or frame_b";
          SI.Velocity der_r_rel_ab[3]
            "Dummy or derivative of relative position vector (resolved in frame_a, frame_b or frame_resolve)";
          SI.AngularVelocity w_rel_ab[3]
            "Dummy or angular velocity of frame_b with respect to frame_a (resolved in frame_a or frame_b)";
          Frames.Orientation R_resolve
            "Dummy or relative orientation of frame_a or frame_b with respect to frame_resolve";

          parameter Integer i1=1;
          parameter Integer i2=if get_r_rel then i1 + 3 else i1;
          parameter Integer i3=if get_v_rel then i2 + 3 else i2;
          parameter Integer i4=if get_a_rel then i3 + 3 else i3;
          parameter Integer i5=if get_angles then i4 + 3 else i4;
          parameter Integer i6=if get_w_rel then i5 + 3 else i5;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Arrow arrow(
            r=frame_a.r_0,
            r_head=frame_b.r_0 - frame_a.r_0,
            diameter=arrowDiameter,
            color=arrowColor,
            specularCoefficient) if world.enableAnimation and animation;
        equation
          if get_angles or get_w_rel or get_z_rel then
            R_rel = Modelica.Mechanics.MultiBody.Frames.relativeRotation(frame_a.R, frame_b.R);
          else
            R_rel = Modelica.Mechanics.MultiBody.Frames.nullRotation();
          end if;

          if get_angles then
            angles = Frames.axesRotationsAngles(R_rel, sequence, guessAngle1);
          else
            angles = zeros(3);
          end if;

          if cardinality(frame_resolve) == 1 then
            // frame_resolve is connected
            frame_resolve.f = zeros(3);
            frame_resolve.t = zeros(3);

            if resolveInFrame_a then
              R_resolve = Frames.relativeRotation(frame_a.R, frame_resolve.R);
            else
              R_resolve = Frames.relativeRotation(frame_b.R, frame_resolve.R);
            end if;

            if get_r_rel or get_v_rel or get_a_rel then
              if resolveInFrame_a then
                r_rel_ab = Frames.resolve2(frame_a.R, frame_b.r_0 - frame_a.r_0);
              else
                r_rel_ab = Frames.resolve2(frame_b.R, frame_b.r_0 - frame_a.r_0);
              end if;
              r_rel = Frames.resolve2(R_resolve, r_rel_ab);
            else
              r_rel_ab = zeros(3);
              r_rel = zeros(3);
            end if;

            if get_v_rel or get_a_rel then
              der_r_rel_ab = der(r_rel_ab);
            else
              der_r_rel_ab = zeros(3);
            end if;

            if get_v_rel then
              v_rel = Frames.resolve2(R_resolve, der_r_rel_ab);
            else
              v_rel = zeros(3);
            end if;

            if get_a_rel then
              a_rel = Frames.resolve2(R_resolve, der(der_r_rel_ab));
            else
              a_rel = zeros(3);
            end if;

            if get_w_rel or get_z_rel then
              if resolveInFrame_a then
                w_rel_ab = Modelica.Mechanics.MultiBody.Frames.angularVelocity1(R_rel);
              else
                w_rel_ab = Modelica.Mechanics.MultiBody.Frames.angularVelocity2(R_rel);
              end if;
              w_rel = Frames.resolve2(R_resolve, w_rel_ab);
            else
              w_rel = zeros(3);
              w_rel_ab = zeros(3);
            end if;

            if get_z_rel then
              z_rel = Frames.resolve2(R_resolve, der(w_rel_ab));
            else
              z_rel = zeros(3);
            end if;

          else
            // frame_resolve is NOT connected
            frame_resolve.r_0 = zeros(3);
            frame_resolve.R = Frames.nullRotation();
            R_resolve = Frames.nullRotation();
            r_rel_ab = zeros(3);
            der_r_rel_ab = zeros(3);
            w_rel_ab = zeros(3);

            if get_r_rel or get_v_rel or get_a_rel then
              if resolveInFrame_a then
                r_rel = Frames.resolve2(frame_a.R, frame_b.r_0 - frame_a.r_0);
              else
                r_rel = Frames.resolve2(frame_b.R, frame_b.r_0 - frame_a.r_0);
              end if;
            else
              r_rel = zeros(3);
            end if;

            if get_v_rel or get_a_rel then
              v_rel = der(r_rel);
            else
              v_rel = zeros(3);
            end if;

            if get_a_rel then
              a_rel = der(v_rel);
            else
              a_rel = zeros(3);
            end if;

            if get_w_rel or get_z_rel then
              if resolveInFrame_a then
                w_rel = Frames.angularVelocity1(R_rel);
              else
                w_rel = Frames.angularVelocity2(R_rel);
              end if;
            else
              w_rel = zeros(3);
            end if;

            if get_z_rel then
              z_rel = der(w_rel);
            else
              z_rel = zeros(3);
            end if;
          end if;

          frame_a.f = zeros(3);
          frame_a.t = zeros(3);
          frame_b.f = zeros(3);
          frame_b.t = zeros(3);

          if get_r_rel then
            y[i1:i1 + 2] = r_rel;
          end if;

          if get_v_rel then
            y[i2:i2 + 2] = v_rel;
          end if;

          if get_a_rel then
            y[i3:i3 + 2] = a_rel;
          end if;

          if get_angles then
            y[i4:i4 + 2] = angles;
          end if;

          if get_w_rel then
            y[i5:i5 + 2] = w_rel;
          end if;

          if get_z_rel then
            y[i6:i6 + 2] = z_rel;
          end if;
          annotation (
            obsolete="Based on a packed result signal which is not a good design. Use instead Modelica.Mechanics.MultiBody.Sensors.RelativeSensor",
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={Line(
                  points={{-60,-94},{-60,-76},{0,-76},{0,-76}},
                  color={95,95,95},
                  pattern=LinePattern.Dot), Text(
                  extent={{-157,-49},{-26,-74}},
                  lineColor={192,192,192},
                  pattern=LinePattern.Dot,
                  textString="resolve")}),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics={Line(
                  points={{-60,-98},{-60,-76},{0,-76},{0,-76}},
                  color={95,95,95},
                  pattern=LinePattern.Dot)}),
            Documentation(info="<HTML>
<p>
Relative kinematic quantities between frame_a and frame_b are
determined and provided at the output signal connector <b>y</b>
in packed format in the order
</p>
<ol>
<li> relative position vector (= r_rel)</li>
<li> relative velocity vector (= v_rel)</li>
<li> relative acceleration vector (= a_rel))</li>
<li> 3 angles to rotate frame_a into frame_b (= angles)</li>
<li> relative angular velocity vector (= w_rel)</li>
<li> relative angular acceleration vector (= z_rel)</li>
</ol>
<p>
For example, if parameters <b>get_v_rel</b> and <b>get_w_rel</b>
are <b>true</b> and all other get_XXX parameters are <b>false</b>, then
y contains 6 elements:
</p>
<pre>
 y = relative velocity
 y = relative angular velocity
</pre>
<p>
In the following figure the animation of a RelativeSensor
component is shown. The light blue coordinate system is
frame_a, the dark blue coordinate system is frame_b, and
the yellow arrow is the animated sensor.
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Sensors/RelativeSensor.png\">

<p>
If parameter <b>resolveInFrame_a</b> = <b>true</b>, then the
provided relative kinematic vectors of frame_b with respect to
frame_a are resolved before differentiation in frame_a. If this
parameter is <b>false</b>, the relative kinematic vectors are
resolved before differentiation in frame_b.
If <b>frame_resolve</b> is connected to another frame, then the
kinematic vector as defined above and/or its required derivatives
are resolved in frame_resolve. Note, derivatives
of relative kinematic quantities are always performed with
respect to frame_a (<b>resolveInFrame_a</b> = <b>true</b>)
or with respect to frame_b (<b>resolveInFrame_a</b> = <b>false</b>).
The resulting vector is then resolved in frame_resolve, if this
connector is connected.
</p>
<p>
For example, if frame_resolve is not
connected and if resolveInFrame_a = <b>false</b>, and
get_v = <b>true</b>, then
</p>
<pre>
  y = v_rel
    = <b>der</b>(r_rel)
</pre>
<p>
is returned (r_rel = resolve2(frame_b.R, frame_b.r_0 - frame_a.r0)), i.e.,
the derivative of the relative distance from frame_a to frame_b,
resolved in frame_b. If frame_resolve is connected, then
</p>
<pre>
  y = v_rel
    = resolve2(frame_resolve.R, <b>der</b>(r_rel))
</pre>
<p>
is returned, i.e., the previous relative velocity vector is
additionally resolved in frame_resolve.
</p>
<p>
Note, the cut-force and the cut-torque in frame_resolve are
always zero, whether frame_resolve is connected or not.
</p>
<p>
If <b>get_angles</b> = <b>true</b>, the 3 angles to rotate frame_a
into frame_b along the axes defined by parameter <b>sequence</b>
are returned. For example, if sequence = {3,1,2} then frame_a is
rotated around angles[1] along the z-axis, afterwards it is rotated
around angles[2] along the x-axis, and finally it is rotated around
angles[3] along the y-axis and is then identical to frame_b.
The 3 angles are returned in the range
</p>
<pre>
    -<font face=\"Symbol\">p</font> &lt;= angles[i] &lt;= <font face=\"Symbol\">p</font>
</pre>
<p>
There are <b>two solutions</b> for \"angles[1]\" in this range.
Via parameter <b>guessAngle1</b> (default = 0) the
returned solution is selected such that |angles[1] - guessAngle1| is
minimal. The relative transformation matrix between frame_a and
frame_b may be in a singular configuration with respect to \"sequence\", i.e.,
there is an infinite number of angle values leading to the same relative
transformation matrix. In this case, the returned solution is
selected by setting angles[1] = guessAngle1. Then angles[2]
and angles[3] can be uniquely determined in the above range.
</p>
<p>
Note, that parameter <b>sequence</b> has the restriction that
only values 1,2,3 can be used and that sequence[1] &ne; sequence[2]
and sequence[2] &ne; sequence[3]. Often used values are:
</p>
<pre>
  sequence = <b>{1,2,3}</b>  // Cardan angle sequence
           = <b>{3,1,3}</b>  // Euler angle sequence
           = <b>{3,2,1}</b>  // Tait-Bryan angle sequence
</pre>
<p>
Exact definition of the returned quantities
(r_rel_ab, R_rel_ab, w_rel_ab are defined below the enumeration):
</p>
<ol>
<li>r_rel is vector r_rel_ab, resolved according to table below.</li>
<li>v_rel is vector <b>der</b>(r_rel_ab), resolved according to table below.</li>
<li>a_rel is vector <b>der</b>(<b>der</b>(r_rel_ab)), resolved according to
            table below.</li>
<li>angles is a vector of 3 angles such that
    R_rel_ab = Frames.axesRotations(sequence, angles).</li>
<li>w_rel is vector w_rel_ab, resolved according to table below.</li>
<li>z_rel is vector <b>der</b>(w_rel_ab), resolved according to table below.</li>
</ol>
<p>
using the auxiliary quantities
</p>
<ol>
<li> r_rel_ab is vector frame_b.r_0 - frame_a.r_0, resolved either in frame_a or
     frame_b according to parameter resolveInFrame_a.</li>
<li> R_rel_ab is orientation object Frames.relativeRotation(frame_a.R, frame_b.R).</li>
<li> w_rel_ab is vector Frames.angularVelocity1(R_rel_ab, der(R_rel_ab)), resolved either
     in frame_a or frame_b according to parameter resolveInFrame_a.</li>
</ol>
<p>
and resolved in the following frame
</p>
<table border=1 cellspacing=0 cellpadding=2>
  <tr><th><b><i>frame_resolve is</i></b></th>
      <th><b><i>resolveInFrame_a =</i></b></th>
      <th><b><i>vector is resolved in</i></b></th>
  </tr>
  <tr><td valign=\"top\">connected</td>
      <td valign=\"top\">true</td>
      <td valign=\"top\"><b>frame_resolve</b></td>
  </tr>
  <tr><td valign=\"top\">connected</td>
      <td valign=\"top\">false</td>
      <td valign=\"top\"><b>frame_resolve</b></td>
  </tr>
  <tr><td valign=\"top\">not connected</td>
      <td valign=\"top\">true</td>
      <td valign=\"top\"><b>frame_a</b></td>
  </tr>
  <tr><td valign=\"top\">not connected</td>
      <td valign=\"top\">false</td>
      <td valign=\"top\"><b>frame_b</b></td>
  </tr>
</table>
</HTML>"));
        end RelativeSensor;

        model CutForceAndTorque
          "Obsolete model. Use instead Modelica.Mechanics.MultiBody.Sensors.CutForceAndTorque"

          import SI = Modelica.SIunits;
          import Modelica.Mechanics.MultiBody.Types;

          extends
            ObsoleteModelica3.Mechanics.MultiBody.Interfaces.PartialCutForceSensor;
          Modelica.Blocks.Interfaces.RealOutput load[6]
            "Cut force and cut torque resolved in frame_a/frame_b or in frame_resolved, if connected"
               annotation (Placement(transformation(
                origin={-80,-110},
                extent={{10,-10},{-10,10}},
                rotation=90)));

          parameter Boolean animation=true
            "= true, if animation shall be enabled (show force and torque arrow)";
          parameter Boolean positiveSign=true
            "= true, if force and torque with positive sign is returned (= frame_a.f/.t), otherwise with negative sign (= frame_b.f/.t)";
          parameter Boolean resolveInFrame_a=true
            "= true, if force and torque are resolved in frame_a/frame_b, otherwise in the world frame (if connector frame_resolve is connected, the force/torque is resolved in frame_resolve)";
          input Real N_to_m(unit="N/m") = 1000
            " Force arrow scaling (length = force/N_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          input Real Nm_to_m(unit="N.m/m") = 1000
            " Torque arrow scaling (length = torque/Nm_to_m)"
            annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter forceDiameter=world.defaultArrowDiameter
            " Diameter of force arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input SI.Diameter torqueDiameter=forceDiameter
            " Diameter of torque arrow" annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color forceColor=Modelica.Mechanics.MultiBody.Types.Defaults.ForceColor
            " Color of force arrow"
            annotation (Dialog(group="if animation = true", enable=animation));
          input Types.Color torqueColor=Modelica.Mechanics.MultiBody.Types.Defaults.TorqueColor
            " Color of torque arrow"
            annotation (Dialog(group="if animation = true", enable=animation));
          input Types.SpecularCoefficient specularCoefficient = world.defaultSpecularCoefficient
            "Reflection of ambient light (= 0: light is completely absorbed)"
            annotation (Dialog(group="if animation = true", enable=animation));

          SI.Force force[3]
            "Cut force resolved in frame_a/frame_b or in frame_resolved, if connected";
          SI.Torque torque[3]
            "Cut torque resolved in frame_a/frame_b or in frame_resolved, if connected";
        protected
          outer Modelica.Mechanics.MultiBody.World world;
          parameter Integer csign=if positiveSign then +1 else -1;
          SI.Position f_in_m[3]=frame_a.f*csign/N_to_m
            "Force mapped from N to m for animation";
          SI.Position t_in_m[3]=frame_a.t*csign/Nm_to_m
            "Torque mapped from Nm to m for animation";
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.Arrow forceArrow(
            diameter=forceDiameter,
            color=forceColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=f_in_m,
            r_head=-f_in_m) if world.enableAnimation and animation;
          Modelica.Mechanics.MultiBody.Visualizers.Advanced.DoubleArrow
            torqueArrow(
            diameter=torqueDiameter,
            color=torqueColor,
            specularCoefficient=specularCoefficient,
            R=frame_b.R,
            r=frame_b.r_0,
            r_tail=t_in_m,
            r_head=-t_in_m) if world.enableAnimation and animation;
        equation
          if cardinality(frame_resolve) == 1 then
            force = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_resolve.R,
              Modelica.Mechanics.MultiBody.Frames.resolve1(frame_a.R, frame_a.f))*csign;
            torque = Modelica.Mechanics.MultiBody.Frames.resolve2(frame_resolve.R,
              Modelica.Mechanics.MultiBody.Frames.resolve1(frame_a.R, frame_a.t))*csign;
          elseif resolveInFrame_a then
            force = frame_a.f*csign;
            torque = frame_a.t*csign;
          else
            force = Modelica.Mechanics.MultiBody.Frames.resolve1(frame_a.R, frame_a.f)*
              csign;
            torque = Modelica.Mechanics.MultiBody.Frames.resolve1(frame_a.R, frame_a.t)
              *csign;
          end if;

          load[1:3] = force;
          load[4:6] = torque;
          annotation (
            obsolete="Based on a packed result signal which is not a good design. Use instead Modelica.Mechanics.MultiBody.Sensors.CutForceAndTorque",
            preferredView="info",
            Documentation(info="<HTML>
<p>
The cut-force and cut-torque acting at the component to which frame_b is
connected are determined and provided at the output signal connector
<b>load</b>:
</p>
<pre>
  load[1:3] = frame_a.f;
  load[4:6] = frame_a.t;
</pre>
<p>
If parameter <b>positiveSign</b> =
<b>false</b>, the negative cut-force and negative
cut-torque is provided (= frame_b.f and frame_b.t).
If <b>frame_resolve</b> is connected to another frame, then the
cut-force and cut-torque are resolved in frame_resolve.
If <b>frame_resolve</b> is <b>not</b> connected then the
coordinate system in which the cut-force and cut-torque is resolved
is defined by parameter <b>resolveInFrame_a</b>.
If this parameter is <b>true</b>, then the
cut-force and cut-torque is resolved in frame_a, otherwise it is
resolved in the world frame.
</p>
<p>
In the following figure the animation of a CutForceAndTorque
sensor is shown. The dark blue coordinate system is frame_b,
and the green arrows are the cut force and the cut torque,
respectively, acting at frame_b and
with negative sign at frame_a.
</p>

<IMG src=\"modelica://Modelica/Resources/Images/Mechanics/MultiBody/Sensors/CutForceAndTorque.png\">
</HTML>"));
        end CutForceAndTorque;
      end Sensors;

      package Types
        "Constants and types with choices, especially to build menus"
        type AngularVelocity_degs = Modelica.Icons.TypeReal(final quantity="AngularVelocity", final unit=
                   "deg/s")
          "Obsolete type. Use Modelica.SIunits.AngularVelocity instead with an appropriate displayUnit"
              annotation (obsolete="Non SI-units should no longer be used. Use Modelica.SIunits.AngularVelocity instead with an appropriate displayUnit");
        type AngularAcceleration_degs2 = Modelica.Icons.TypeReal (final
              quantity =                                                         "AngularAcceleration",
              final unit="deg/s2")
          "Obsolete type. Use Modelica.SIunits.AngularAcceleration instead with an appropriate displayUnit"
              annotation (obsolete="Non SI-units should no longer be used. Use Modelica.SIunits.AngularAcceleration instead with an appropriate displayUnit");
        package Init
          "Obsolete type. This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu"

          extends ObsoleteModelica3.Icons.Enumeration;

          constant Integer Free=1;
          constant Integer PositionVelocity=2;
          constant Integer SteadyState=3;
          constant Integer Position=4;
          constant Integer Velocity=5;
          constant Integer VelocityAcceleration=6;
          constant Integer PositionVelocityAcceleration=7;

          type Temp
            "Obsolete type. This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu"

            extends Modelica.Icons.TypeInteger;
            annotation (
                obsolete="This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu",
                choices(
                choice=Modelica.Mechanics.MultiBody.Types.Init.Free
                  "free (no initialization)",
                choice=Modelica.Mechanics.MultiBody.Types.Init.PositionVelocity
                  "initialize generalized position and velocity variables",
                choice=Modelica.Mechanics.MultiBody.Types.Init.SteadyState
                  "initialize in steady state (velocity and acceleration are zero)",
                choice=Modelica.Mechanics.MultiBody.Types.Init.Position
                  "initialize only generalized position variable(s)",
                choice=Modelica.Mechanics.MultiBody.Types.Init.Velocity
                  "initialize only generalized velocity variable(s)",
                choice=Modelica.Mechanics.MultiBody.Types.Init.VelocityAcceleration
                  "initialize generalized velocity and acceleration variables",
                choice=Modelica.Mechanics.MultiBody.Types.Init.PositionVelocityAcceleration
                  "initialize generalized position, velocity and acceleration variables"),
                Documentation(info="<html>

<table border=1 cellspacing=0 cellpadding=2>
<tr><th><b>Types.Init.</b></th><th><b>Meaning</b></th></tr>
<tr><td valign=\"top\">Free</td>
    <td valign=\"top\">No initialization</td></tr>

<tr><td valign=\"top\">PositionVelocity</td>
    <td valign=\"top\">Initialize generalized position and velocity variables</td></tr>

<tr><td valign=\"top\">SteadyState</td>
    <td valign=\"top\">Initialize in steady state (velocity and acceleration are zero)</td></tr>

<tr><td valign=\"top\">Position </td>
    <td valign=\"top\">Initialize only generalized position variable(s)</td></tr>

<tr><td valign=\"top\">Velocity</td>
    <td valign=\"top\">Initialize only generalized velocity variable(s)</td></tr>

<tr><td valign=\"top\">VelocityAcceleration</td>
    <td valign=\"top\">Initialize generalized velocity and acceleration variables</td></tr>

<tr><td valign=\"top\">PositionVelocityAcceleration</td>
    <td valign=\"top\">Initialize generalized position, velocity and acceleration variables</td></tr>

</table>

</html>"));

          end Temp;
          annotation (obsolete="This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu",
        Documentation(info="<html>

</html>"));
        end Init;
      end Types;

    end MultiBody;

    package Rotational
      "Library to model 1-dimensional, rotational mechanical systems"

      package Interfaces
        "Connectors and partial models for 1D rotational mechanical components"
        partial model Rigid
          "Base class for the rigid connection of two rotational 1D flanges"
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.SIunits.Angle phi
            "Absolute rotation angle of component (= flange_a.phi = flange_b.phi)";

          Modelica.Mechanics.Rotational.Interfaces.Flange_a flange_a
            "(left) driving flange (flange axis directed INTO cut plane)"
            annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
          Modelica.Mechanics.Rotational.Interfaces.Flange_b flange_b
            "(right) driven flange (flange axis directed OUT OF cut plane)"
            annotation (Placement(transformation(extent={{90,-10},{110,10}})));
        equation
          flange_a.phi = phi;
          flange_b.phi = phi;
          annotation (
            Documentation(info="<html>
<p>
This is a 1D rotational component with two rigidly connected flanges,
i.e., flange_a.phi = flange_b.phi. It is used e.g., to built up components
with inertia.
</p>

</html>"));
        end Rigid;

        partial model Bearing
          "Obsolete model. Use one of Modelica.Mechanics.Rotational.Interfaces.PartialXXX instead"
          extends Modelica.Mechanics.Rotational.Interfaces.PartialTwoFlanges;
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.SIunits.Torque tau_support;

          Modelica.Mechanics.Rotational.Interfaces.Flange_a bearing
            "Flange of bearing"
                           annotation (Placement(transformation(extent={{-10,-110},
                    {10,-90}})));
          annotation (
            obsolete=
                "The Rotational library has now a new improved design with optional support connectors. Use Modelica.Mechanics.Rotational.Interfaces.PartialXXX instead.",
            Diagram(coordinateSystem(preserveAspectRatio=true, extent={{-100,
                    -100},{100,100}}), graphics={Rectangle(
                  extent={{-20,-80},{20,-120}},
                  lineColor={192,192,192},
                  fillColor={192,192,192},
                  fillPattern=FillPattern.Solid)}),
            Icon(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},
                    {100,100}}), graphics={Rectangle(
                  extent={{-20,-80},{20,-120}},
                  lineColor={192,192,192},
                  fillColor={192,192,192},
                  fillPattern=FillPattern.Solid)}),
            Documentation(info="<html>
<p>
This is a 1D rotational component with two flanges and an additional bearing flange.
It is a superclass for the two components TwoFlangesAndBearing and TwoFlangesAndBearingH.</p>

</html>"));

        end Bearing;

        partial model TwoFlangesAndBearing
          "Obsolete model. Use one of Modelica.Mechanics.Rotational.Interfaces.PartialXXX instead"

          extends ObsoleteModelica3.Mechanics.Rotational.Interfaces.Bearing;

          Modelica.SIunits.Angle phi_a;
          Modelica.SIunits.Angle phi_b;

        equation
          if cardinality(bearing) == 0 then
            bearing.phi = 0;
          else
            bearing.tau = tau_support;
          end if;

          0 = flange_a.tau + flange_b.tau + tau_support;

          phi_a = flange_a.phi - bearing.phi;
          phi_b = flange_b.phi - bearing.phi;
          annotation (
            obsolete=
                "The Rotational library has now a new improved design with optional support connectors. Use Modelica.Mechanics.Rotational.Interfaces.PartialXXX instead.",
          Documentation(info="<html>
<p>
This is a 1D rotational component with two flanges and an additional bearing flange.
It is used e.g., to build up equation-based parts of a drive train.</p>

</html>"));
        end TwoFlangesAndBearing;

        partial model TwoFlangesAndBearingH
          "Obsolete model. Use one of Modelica.Mechanics.Rotational.Interfaces.PartialXXX instead"

          extends ObsoleteModelica3.Mechanics.Rotational.Interfaces.Bearing;

          Adapter adapter(bearingConnected=cardinality(bearing) > 1)
            annotation (Placement(transformation(
                origin={0,-60},
                extent={{-10,-10},{10,10}},
                rotation=90)));
        protected
          encapsulated model Adapter
            import Modelica;
            import ObsoleteModelica3;
            import TwoFlanges =
              Modelica.Mechanics.Rotational.Interfaces.PartialTwoFlanges;
            extends Modelica.Mechanics.Rotational.Interfaces.PartialTwoFlanges;
            extends ObsoleteModelica3.Icons.ObsoleteModel;
            parameter Boolean bearingConnected;

          equation
            flange_a.phi = flange_b.phi;

            if bearingConnected then
              0 = flange_a.tau + flange_b.tau;
            else
              0 = flange_a.phi;
            end if;
            annotation (Icon(coordinateSystem(preserveAspectRatio=true, extent={{-100,
                      -100},{100,100}}), graphics={Rectangle(
                    extent={{-90,10},{90,-10}},
                    lineColor={192,192,192},
                    fillColor={192,192,192},
                    fillPattern=FillPattern.Solid), Text(
                    extent={{-150,60},{150,20}},
                    textString="%name",
                    lineColor={0,0,255})}));
          end Adapter;
        equation
          tau_support = -adapter.flange_b.tau;
          connect(adapter.flange_a, bearing) annotation (Line(points={{
                  0,-70},{0,-70},{0,-100}}));
          annotation (obsolete=
                "The Rotational library has now a new improved design with optional support connectors. Use Modelica.Mechanics.Rotational.Interfaces.PartialXXX instead.",
        Documentation(info="<html>
<p>
This is a 1D rotational component with two flanges and an additional bearing flange.
It is used e.g., to build up parts of a drive train consisting
of several base components.</p>

</html>"));
        end TwoFlangesAndBearingH;

        partial model PartialSpeedDependentTorque
          "Partial model of a torque acting at the flange (accelerates the flange)"
          extends ObsoleteModelica3.Icons.ObsoleteModel;
          Modelica.SIunits.AngularVelocity w = der(flange.phi)
            "Angular velocity at flange";
          Modelica.SIunits.Torque tau = flange.tau
            "accelerating torque acting at flange";
          Modelica.Mechanics.Rotational.Interfaces.Flange_b flange
            "Flange on which torque is acting"
            annotation (Placement(transformation(extent={{110,-10},{90,10}})));
          Modelica.Mechanics.Rotational.Interfaces.Flange_a bearing
            "Bearing at which the reaction torque (i.e., -flange.tau) is acting"
               annotation (Placement(transformation(extent={{-10,-130},{10,-110}})));
        equation
          if cardinality(bearing) == 0 then
            bearing.phi = 0;
          else
            bearing.tau = -flange.tau;
          end if;
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {100,100}}), graphics={
                Rectangle(
                  extent={{-96,96},{96,-96}},
                  lineColor={255,255,255},
                  fillColor={255,255,255},
                  fillPattern=FillPattern.Solid),
                Line(points={{-30,-70},{30,-70}}),
                Line(points={{-30,-90},{-10,-70}}),
                Line(points={{-10,-90},{10,-70}}),
                Rectangle(
                  extent={{-20,-100},{20,-140}},
                  lineColor={192,192,192},
                  fillColor={192,192,192},
                  fillPattern=FillPattern.Solid),
                Line(points={{10,-90},{30,-70}}),
                Line(points={{0,-70},{0,-110}}),
                Line(points={{-92,0},{-76,36},{-54,62},{-30,80},{-14,88},{10,92},
                      {26,90},{46,80},{64,62}}),
                Text(
                  extent={{-150,140},{150,100}},
                  lineColor={0,0,255},
                  textString=
                       "%name"),
                Polygon(
                  points={{94,16},{80,74},{50,52},{94,16}},
                  lineColor={0,0,0},
                  fillColor={0,0,0},
                  fillPattern=FillPattern.Solid)}),
            Documentation(info="<HTML>
<p>
Partial model of torque dependent on speed that accelerates the flange.
</p>
</HTML>"));
        end PartialSpeedDependentTorque;

        partial model AbsoluteSensor
          "Obsolete model. Use Modelica.Mechanics.Rotational.Interfaces.PartialAbsoluteSensor instead and define a meaningful name for the output signal"

          extends Modelica.Icons.RotationalSensor;
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.Mechanics.Rotational.Interfaces.Flange_a flange_a
            "(left) flange to be measured (flange axis directed INTO cut plane)"
            annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
          Modelica.Blocks.Interfaces.RealOutput y "Sensor signal"
            annotation (Placement(transformation(extent={{100,-10},{120,10}})));
          annotation (
            obsolete=
                "Use Modelica.Mechanics.Rotational.Interfaces.PartialAbsoluteSensor instead and define a meaningful name for the output signal.",
            Documentation(info="<html>
<p>
This is the base class of a 1D rotational component with one flange and one
output signal y in order to measure an absolute kinematic quantity in the flange
and to provide the measured signal as output signal for further processing
with the blocks of package Modelica.Blocks.
</p>

</html>"),         Icon(coordinateSystem(
                preserveAspectRatio=true,
                extent={{-100,-100},{100,100}}),
                graphics={
                Line(points={{-70,0},{-90,0}}),
                Line(points={{70,0},{100,0}}, color={0,0,127}),
                Text(
                  extent={{150,80},{-150,120}},
                  textString="%name",
                  lineColor={0,0,255})}),
            Diagram(coordinateSystem(
                preserveAspectRatio=true,
                extent={{-100,-100},{100,100}}),
                graphics={Line(points={{-70,0},{-90,0}}, color={0,
                      0,0}), Line(points={{70,0},{100,0}}, color={0,0,255})}));
        end AbsoluteSensor;

        partial model RelativeSensor
          "Obsolete model. Use Modelica.Mechanics.Rotational.Interfaces.PartialRelativbeSensor instead and define a meaningful name for the output signal"

          extends Modelica.Icons.RotationalSensor;
          extends ObsoleteModelica3.Icons.ObsoleteModel;

          Modelica.Mechanics.Rotational.Interfaces.Flange_a flange_a
            "(left) driving flange (flange axis directed INTO cut plane)"
            annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
          Modelica.Mechanics.Rotational.Interfaces.Flange_b flange_b
            "(right) driven flange (flange axis directed OUT OF cut plane)"
            annotation (Placement(transformation(extent={{90,-10},{110,10}})));
          Modelica.Blocks.Interfaces.RealOutput y "Sensor signal"
            annotation (Placement(transformation(
                origin={0,-110},
                extent={{10,-10},{-10,10}},
                rotation=90)));
          annotation (
            obsolete=
                "Use Modelica.Mechanics.Rotational.Interfaces.PartialRelativeSensor instead and define a meaningful name for the output signal.",
            Documentation(info="<html>
<p>
This is a base class for 1D rotational components with two rigidly connected
flanges and one output signal y in order to measure relative kinematic quantities
between the two flanges or the cut-torque in the flange and
to provide the measured signal as output signal for further processing
with the blocks of package Modelica.Blocks.
</p>

</html>"),         Icon(coordinateSystem(
                preserveAspectRatio=true,
                extent={{-100,-100},{100,100}}),
                graphics={
                Line(points={{-70,0},{-90,0}}),
                Line(points={{70,0},{90,0}}),
                Line(points={{0,-100},{0,-70}}, color={0,0,127}),
                Text(
                  extent={{-150,70},{150,110}},
                  textString="%name",
                  lineColor={0,0,255})}),
            Diagram(coordinateSystem(
                preserveAspectRatio=true,
                extent={{-100,-100},{100,100}}),
                graphics={
                Line(points={{-70,0},{-90,0}}),
                Line(points={{70,0},{90,0}}),
                Line(points={{0,-100},{0,-70}}, color={0,0,255})}));
        end RelativeSensor;
      end Interfaces;

      package Types
        "Constants and types with choices, especially to build menus"
        extends Modelica.Icons.Library;

        package Init
          "Obsolete type. This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu"
          extends ObsoleteModelica3.Icons.Enumeration;
          constant Integer NoInit=1
            "no initialization (phi_start, w_start are guess values)";
          constant Integer SteadyState=2
            "steady state initialization (der(phi)=der(w)=0)";
          constant Integer InitialState=3
            "initialization with phi_start, w_start";
          constant Integer InitialAngle=4 "initialization with phi_start";
          constant Integer InitialSpeed=5 "initialization with w_start";
          constant Integer InitialAcceleration=6 "initialization with a_start";
          constant Integer InitialAngleAcceleration=7
            "initialization with phi_start, a_start";
          constant Integer InitialSpeedAcceleration=8
            "initialization with w_start, a_start";
          constant Integer InitialAngleSpeedAcceleration=9
            "initialization with phi_start, w_start, a_start";

          type Temp
            "Obsolete type. This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu"
            extends Modelica.Icons.TypeInteger(min=1,max=9);

            annotation (
                  obsolete="This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu",
                Evaluate=true, choices(
                choice=Modelica.Mechanics.Rotational.Types.Init.NoInit
                  "no initialization (phi_start, w_start are guess values)",
                choice=Modelica.Mechanics.Rotational.Types.Init.SteadyState
                  "steady state initialization (der(phi)=der(w)=0)",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialState
                  "initialization with phi_start, w_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialAngle
                  "initialization with phi_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialSpeed
                  "initialization with w_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialAcceleration
                  "initialization with a_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialAngleAcceleration
                  "initialization with phi_start, a_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialSpeedAcceleration
                  "initialization with w_start, a_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialAngleSpeedAcceleration
                  "initialization with phi_start, w_start, a_start"));
          end Temp;

          annotation (Documentation(info="<html>
<p>
Type <b>Init</b> defines initialization of absolute rotational
quantities.
</p>

</html>"));
        end Init;

        package InitRel
          "Obsolete type. This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu"
          extends ObsoleteModelica3.Icons.Enumeration;
          constant Integer NoInit=1
            "no initialization (phi_rel_start, w_rel_start are guess values)";
          constant Integer SteadyState=2
            "steady state initialization (der(phi_rel)=der(w_rel)=0)";
          constant Integer InitialState=3
            "initialization with phi_rel_start, w_rel_start";
          constant Integer InitialAngle=4 "initialization with phi_rel_start";
          constant Integer InitialSpeed=5 "initialization with w_rel_start";

          type Temp
            "Obsolete type. This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu"
            extends Modelica.Icons.TypeInteger(min=1,max=5);

            annotation (
                 obsolete="This is an emulated enumeration for initialization. Initialization is now defined with start/fixed values and appropriate support in the parameter menu",
                Evaluate=true, choices(
                choice=Modelica.Mechanics.Rotational.Types.Init.NoInit
                  "no initialization (phi_rel_start, w_rel_start are guess values)",
                choice=Modelica.Mechanics.Rotational.Types.Init.SteadyState
                  "steady state initialization (der(phi)=der(w)=0)",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialState
                  "initialization with phi_rel_start, w_rel_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialAngle
                  "initialization with phi_rel_start",
                choice=Modelica.Mechanics.Rotational.Types.Init.InitialSpeed
                  "initialization with w_rel_start"));
          end Temp;

          annotation (Documentation(info="<html>
<p>
Type <b>Init</b> defines initialization of relative rotational
quantities.
</p>

</html>"));
        end InitRel;
        annotation (preferredView="info", Documentation(info="<HTML>
<p>
In this package <b>types</b> and <b>constants</b> are defined that are used
in library Modelica.Blocks. The types have additional annotation choices
definitions that define the menus to be built up in the graphical
user interface when the type is used as parameter in a declaration.
</p>
</HTML>"));
      end Types;

      model GearEfficiency
        "Obsolete model. Use Modelica.Mechanics.Rotational.Components.LossyGear instead"
        extends
          ObsoleteModelica3.Mechanics.Rotational.Interfaces.TwoFlangesAndBearing;
        extends ObsoleteModelica3.Icons.ObsoleteModel;

        parameter Real eta(
          min=Modelica.Constants.small,
          max=1) = 1 "Efficiency";
        Modelica.SIunits.Angle phi;
        Modelica.SIunits.Power power_a "Energy flowing into flange_a (= power)";
        Boolean driving_a
          "True, if energy is flowing INTO and not out of flange flange_a";

      equation
        phi = phi_a;
        phi = phi_b;
        power_a = flange_a.tau*der(phi);
        driving_a = power_a >= 0;
        flange_b.tau = -(if driving_a then eta*flange_a.tau else flange_a.tau/eta);
        annotation (
          obsolete=
              "This model can get stuck due when the torque direction varies, use Modelica.Mechanics.Rotational.Components.LossyGear instead.",
          Icon(coordinateSystem(
              preserveAspectRatio=true,
              extent={{-100,-100},{100,100}}),
              graphics={
              Text(
                extent={{-150,100},{150,60}},
                textString="%name",
                lineColor={0,0,255}),
              Rectangle(
                extent={{-100,20},{100,-20}},
                lineColor={0,0,0},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={192,192,192}),
              Line(points={{-30,-40},{30,-40}}),
              Line(points={{0,-40},{0,-90}}),
              Polygon(
                points={{-30,-20},{60,-20},{60,-80},{70,-80},{50,-100},{30,-80},
                    {40,-80},{40,-30},{-30,-30},{-30,-20},{-30,-20}},
                lineColor={0,0,0},
                fillColor={255,0,0},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{-150,60},{150,20}},
                lineColor={0,0,0},
                textString="eta=%eta"),
              Line(points={{30,-50},{20,-60}}),
              Line(points={{30,-40},{10,-60}}),
              Line(points={{20,-40},{0,-60}}),
              Line(points={{10,-40},{-10,-60}}),
              Line(points={{0,-40},{-20,-60}}),
              Line(points={{-10,-40},{-30,-60}}),
              Line(points={{-20,-40},{-30,-50}})}),
          Documentation(info="<html>
<p>
THIS COMPONENT IS <b>OBSOLETE</b> and should <b>no longer be used</b>. It is only
kept for <b>backward compatibility</b> purposes. Use model
Modelica.Mechanics.Rotational.LossyGear instead which implements
gear efficiency in a much more reliable way.
</p>
<p>
This component consists of two rigidly connected flanges flange_a and flange_b without
inertia where an <b>efficiency</b> coefficient <b>eta</b> reduces the driven
torque as function of the driving torque depending on the direction
of the energy flow, i.e., energy is always lost. This can be seen as a
simple model of the Coulomb friction acting between the teeth of a
gearbox.
</p>
<p>
Note, that most gearbox manufacturers provide tables of the
efficiency of a gearbox as function of the angular velocity
(efficiency becomes zero, if the angular velocity is zero).
However, such a table is practically useless for simulation purposes,
because in gearboxes always two types of friction is present:
(1) Friction in the <b>bearings</b> and (2) friction between
the teeth of the gear. (1) leads to a velocity dependent, additive
loss-torque, whereas (2) leads to a torque-dependent reduction of the
driving torque. The gearbox manufacturers measure both effects
together and determine the gear efficiency from it, although for
simulation purposes the two effects need to be separated.
Assume for example that only constant bearing friction, i.e.,
bearingTorque=const., is present, i.e.,
</p>
<pre>
   (1)  loadTorque = motorTorque - sign(w)*bearingTorque
</pre>
<p>
Gearbox manufacturers use the loss-formula
</p>
<pre>
   (2)  loadTorque = eta*motorTorque
</pre>
<p>
Comparing (1) and (2) gives a formula for the efficiency eta:
</p>
<pre>
   eta = (1 - sign(w)*bearingTorque/motorTorque)
</pre>
<p>
When the motorTorque becomes smaller as the bearingTorque,
(2) is useless, because the efficiency is zero. To summarize,
be careful to determine the gear <b>efficiency</b> of this element
from tables of the gear manufacturers.
</p>

</html>"),       Diagram(coordinateSystem(
              preserveAspectRatio=true,
              extent={{-100,-100},{100,100}}),
              graphics={
              Rectangle(
                extent={{-96,20},{96,-21}},
                lineColor={0,0,0},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={192,192,192}),
              Line(points={{-30,-40},{30,-40}}),
              Line(points={{0,60},{0,40}}),
              Line(points={{-30,40},{29,40}}),
              Line(points={{0,-40},{0,-90}}),
              Polygon(
                points={{-30,-20},{60,-20},{60,-80},{70,-80},{50,-100},{30,-80},
                    {40,-80},{40,-30},{-30,-30},{-30,-20},{-30,-20}},
                lineColor={0,0,0},
                fillColor={255,0,0},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{16,83},{84,70}},
                lineColor={128,128,128},
                textString="rotation axis"),
              Polygon(
                points={{12,76},{-8,81},{-8,71},{12,76}},
                lineColor={128,128,128},
                fillColor={128,128,128},
                fillPattern=FillPattern.Solid),
              Line(points={{-78,76},{-7,76}}, color={128,128,128}),
              Line(points={{30,-50},{20,-60}}),
              Line(points={{30,-40},{10,-60}}),
              Line(points={{20,-40},{0,-60}}),
              Line(points={{10,-40},{-10,-60}}),
              Line(points={{0,-40},{-20,-60}}),
              Line(points={{-10,-40},{-30,-60}}),
              Line(points={{-20,-40},{-30,-50}})}));
      end GearEfficiency;

      model Gear
        "Obsolete model. Use Modelica.Mechanics.Rotational.Components.Gearbox instead"
        extends
          ObsoleteModelica3.Mechanics.Rotational.Interfaces.TwoFlangesAndBearingH;

        parameter Real ratio=1 "transmission ratio (flange_a.phi/flange_b.phi)";
        parameter Real eta(
          min=Modelica.Constants.small,
          max=1) = 1 "Gear efficiency";
        parameter Real friction_pos[:, 2]=[0, 1]
          "[w,tau] positive sliding friction characteristic (w>=0)";
        parameter Real peak(final min=1) = 1
          "peak*friction_pos[1,2] = maximum friction torque at zero velocity";
        parameter Real c(
          final unit="N.m/rad",
          final min=Modelica.Constants.small) = 1.e5
          "Gear elasticity (spring constant)";
        parameter Real d(
          final unit="N.m.s/rad",
          final min=0) = 0 "(relative) gear damping";
        parameter Modelica.SIunits.Angle b(final min=0)=0 "Total backlash";

        Modelica.Mechanics.Rotational.Components.IdealGear gearRatio(final
            ratio =                                                              ratio)
          annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
        ObsoleteModelica3.Mechanics.Rotational.GearEfficiency gearEfficiency(
                                      final eta=eta)
          annotation (Placement(transformation(extent={{-30,-10},{-10,10}})));
        Modelica.Mechanics.Rotational.Components.ElastoBacklash elastoBacklash(
          final b=b,
          final c=c,
          final phi_rel0=0,
          final d=d) annotation (Placement(transformation(extent={{50,-10},{70,10}})));
        Modelica.Mechanics.Rotational.Components.BearingFriction
          bearingFriction(                                                       final
            tau_pos=friction_pos, final peak=peak)
          annotation (Placement(transformation(extent={{10,-10},{30,10}})));
      equation
        connect(flange_a, gearRatio.flange_a)
          annotation (Line(points={{-100,0},{-70,0}}));
        connect(gearRatio.flange_b, gearEfficiency.flange_a)
          annotation (Line(points={{-50,0},{-30,0}}));
        connect(gearEfficiency.flange_b, bearingFriction.flange_a)
          annotation (Line(points={{-10,0},{10,0}}));
        connect(bearingFriction.flange_b, elastoBacklash.flange_a)
          annotation (Line(points={{30,0},{50,0}}));
        connect(elastoBacklash.flange_b, flange_b)
          annotation (Line(points={{70,0},{100,0}}));
        connect(gearEfficiency.bearing, adapter.flange_b) annotation (Line(points={{-20,-10},
                {-20,-40},{0,-40},{0,-50}},           color={
                0,0,0}));
        connect(bearingFriction.support, adapter.flange_b) annotation (Line(
            points={{20,-10},{20,-40},{0,-40},{0,-50}}));
        connect(gearRatio.support, adapter.flange_b) annotation (Line(
            points={{-60,-10},{-60,-40},{0,-40},{0,-50}}));

        annotation (
          obsolete=
              "This model can get stuck due when the torque direction varies, use Modelica.Mechanics.Rotational.Components.Gearbox instead.",
          Documentation(info="<html>
<p>
This component models the essential effects of a gearbox, in particular
gear <b>efficiency</b> due to friction between the teeth, <b>bearing friction</b>,
gear <b>elasticity</b> and <b>damping</b>, <b>backlash</b>.
The inertia of the gear wheels is not modeled. If necessary, inertia
has to be taken into account by connecting components of model Inertia
to the left and/or the right flange.
</p>

</html>"),       Icon(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},{
                  100,100}}), graphics={
              Rectangle(
                extent={{-40,60},{40,-60}},
                lineColor={0,0,0},
                lineThickness=0.25,
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={192,192,192}),
              Polygon(
                points={{-60,-80},{-46,-80},{-20,-20},{20,-20},{46,-80},{60,-80},
                    {60,-90},{-60,-90},{-60,-80}},
                lineColor={0,0,0},
                fillColor={0,0,0},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-100,10},{-60,-10}},
                lineColor={0,0,0},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={192,192,192}),
              Rectangle(
                extent={{60,10},{100,-10}},
                lineColor={0,0,0},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={192,192,192}),
              Polygon(
                points={{-60,10},{-60,20},{-40,40},{-40,-40},{-60,-20},{-60,10}},
                lineColor={0,0,0},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={128,128,128}),
              Polygon(
                points={{60,20},{40,40},{40,-40},{60,-20},{60,20}},
                lineColor={128,128,128},
                fillColor={128,128,128},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{-150,110},{150,70}},
                textString="%name=%ratio",
                lineColor={0,0,255}),
              Text(
                extent={{-150,-160},{150,-120}},
                lineColor={0,0,0},
                textString="c=%c")}),
          Diagram(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},
                  {100,100}}), graphics={
              Text(
                extent={{2,29},{46,22}},
                lineColor={128,128,128},
                textString="rotation axis"),
              Polygon(
                points={{4,25},{-4,27},{-4,23},{4,25}},
                lineColor={128,128,128},
                fillColor={128,128,128},
                fillPattern=FillPattern.Solid),
              Line(points={{-36,25},{-3,25}}, color={128,128,128})}));
      end Gear;
    end Rotational;
  end Mechanics;
  annotation (uses(Modelica(version="3.2.2")),
Documentation(info="<html>
<p>
This package contains models and blocks from the Modelica Standard Library
version 2.2.2 that are no longer available in version 3.0.
The conversion script for version 3.0 changes references in existing
user models automatically to the models and blocks of package
ObsoleteModelica3. The user should <b>manually</b> replace all
references to ObsoleteModelica3 in his/her models to the models
that are recommended in the documentation of the respective model.
</p>

<p>
In most cases, this means that a model with the name
\"ObsoleteModelica3.XXX\" should be renamed to \"Modelica.XXX\" (version 3.0)
and then a manual adaptation is needed. For example, a reference to
ObsoleteModelica3.Mechanics.MultiBody.Sensors.AbsoluteSensor
should be replaced by
Modelica.Mechanics.MultiBody.Sensors.AbsoluteSensor (version 3.0).
Since the design of the component has changed (e.g., several
optional connectors, and no longer one connector where all signals
are packed together), this requires some changes at the place where
the model is used (besides the renaming of the underlying class).
</p>

<p>
The models in ObsoleteModelica3 are either not according to the Modelica Language
version 3.0 and higher, or the model was changed to get a better design.
In all cases, an automatic conversion to the new implementation
was not feasible, since too complicated.
</p>

<p>
In order to easily detect obsolete models and blocks, all of them are specially
marked in the icon layer with a red box.
</p>

<p>
Copyright &copy; 2007-2016, Modelica Association.
</p>
<p>
<i>This Modelica package is <u>free</u> software and the use is completely at <u>your own risk</u>; it can be redistributed and/or modified under the terms of the Modelica License 2. For license conditions (including the disclaimer of warranty) see <a href=\"modelica://Modelica.UsersGuide.ModelicaLicense2\">Modelica.UsersGuide.ModelicaLicense2</a> or visit <a href=\"https://www.modelica.org/licenses/ModelicaLicense2\"> https://www.modelica.org/licenses/ModelicaLicense2</a>.</i>
</p>
</html>"));
end ObsoleteModelica3;
