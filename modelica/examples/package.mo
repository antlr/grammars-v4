within ;
package Modelica "Modelica Standard Library - Version 3.2.2"
extends Modelica.Icons.Package;


package UsersGuide "User's Guide"
  extends Modelica.Icons.Information;

class Overview "Overview of Modelica Library"
  extends Modelica.Icons.Information;

 annotation (Documentation(info="<html>
<p>
The Modelica Standard Library consists of the following
main sub-libraries:
</p>

<table border=1 cellspacing=0 cellpadding=2>
<tr><th>Library Components</th> <th>Description</th></tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Electrical.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Electrical.Analog\">Analog</a><br>
 Analog electric and electronic components, such as
 resistor, capacitor, transformers, diodes, transistors,
 transmission lines, switches, sources, sensors.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Digital.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Electrical.Digital\">Digital</a><br>
 Digital electrical components based on the VHDL standard,
 like basic logic blocks with 9-value logic, delays, gates,
 sources, converters between 2-, 3-, 4-, and 9-valued logic.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Machines.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Electrical.Machines\">Machines</a><br>
            Electrical asynchronous-, synchronous-, and DC-machines
 (motors and generators) as well as 3-phase transformers.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-FluxTubes.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Magnetic.FluxTubes\">FluxTubes</a><br>
Based on magnetic flux tubes concepts. Especially to model electro-magnetic actuators. Nonlinear shape, force, leakage, and material models. Material data for steel, electric sheet, pure iron, Cobalt iron, Nickel iron, NdFeB, Sm2Co17, and more.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Translational.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Mechanics.Translational\">Translational</a><br>
 1-dim. mechanical, translational systems, e.g.,
 sliding mass, mass with stops, spring, damper.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Rotational.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Mechanics.Rotational\">Rotational</a><br>
 1-dim. mechanical, rotational systems, e.g., inertias, gears,
 planetary gears, convenient definition of speed/torque dependent friction
 (clutches, brakes, bearings, ..)
 </td>
</tr>

<tr><td valign=\"top\" width=100>
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-MultiBody1.png\"><br>
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-MultiBody2.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Mechanics.MultiBody\">MultiBody</a>
 3-dim. mechanical systems consisting of joints, bodies, force and
 sensor elements. Joints can be driven by drive trains defined by
 1-dim. mechanical system library (Rotational).
 Every component has a default animation.
 Components can be arbitrarily connected together.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Fluid.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Fluid\">Fluid</a><br>
        1-dim. thermo-fluid flow in networks of vessels, pipes,
        fluid machines, valves and fittings. All media from the
        Modelica.Media library can be used (so incompressible or compressible,
        single or multiple substance, one or two phase medium).
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Media.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Media\">Media</a><br>
 Large media library providing models and functions
 to compute media properties, such as h = h(p,T), d = d(p,T),
 for the following media:
 <ul>
 <li> 1240 gases and mixtures between these gases.</li>
 <li> incompressible, table based liquids (h = h(T), etc.).</li>
 <li> compressible liquids</li>
 <li> dry and moist air</li>
 <li> high precision model for water (IF97).</li>
 </ul>
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Thermal.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Thermal.FluidHeatFlow\">FluidHeatFlow</a>,
 <a href=\"modelica://Modelica.Thermal.HeatTransfer\">HeatTransfer</a>
 Simple thermo-fluid pipe flow, especially to model cooling of machines
 with air or water (pipes, pumps, valves, ambient, sensors, sources) and
 lumped heat transfer with heat capacitors, thermal conductors, convection,
 body radiation, sources and sensors.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Blocks1.png\"><br>
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-Blocks2.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Blocks\">Blocks</a><br>
 Input/output blocks to model block diagrams and logical networks, e.g.,
 integrator, PI, PID, transfer function, linear state space system,
 sampler, unit delay, discrete transfer function, and/or blocks,
 timer, hysteresis, nonlinear and routing blocks, sources, tables.
 </td>
</tr>

<tr><td valign=\"top\">
 <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Lib-StateGraph.png\">
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.StateGraph\">StateGraph</a><br>
 Hierarchical state machines with a similar modeling power as Statecharts.
 Modelica is used as synchronous action language, i.e., deterministic
 behavior is guaranteed
 </td>
</tr>

<tr><td valign=\"top\">
 <pre>
 A = [1,2,3;
   3,4,5;
   2,1,4];
 b = {10,22,12};
 x = Matrices.solve(A,b);
 Matrices.eigenValues(A);
 </pre>
 </td>
 <td valign=\"top\">
 <a href=\"modelica://Modelica.Math\">Math</a>,
 <a href=\"modelica://Modelica.Utilities\">Utilities</a><br>
 Functions operating on vectors and matrices, such as for solving
 linear systems, eigen and singular values etc.,  and
 functions operating on strings, streams, files, e.g.,
 to copy and remove a file or sort a vector of strings.
 </td>
</tr>

</table>

</html>"));
end Overview;

class Connectors "Connectors"
  extends Modelica.Icons.Information;

 annotation (Documentation(info="<html>

<p>
The Modelica standard library defines the most important
<b>elementary connectors</b> in various domains. If any possible,
a user should utilize these connectors in order that components
from the Modelica Standard Library and from other libraries
can be combined without problems.
The following elementary connectors are defined
(the meaning of potential, flow, and stream
variables is explained in section \"Connector Equations\" below):
</p>

<table border=1 cellspacing=0 cellpadding=1>
<tr><td valign=\"top\"><b>domain</b></td>
   <td valign=\"top\"><b>potential<br>variables</b></td>
   <td valign=\"top\"><b>flow<br>variables</b></td>
   <td valign=\"top\"><b>stream<br>variables</b></td>
   <td valign=\"top\"><b>connector definition</b></td>
   <td valign=\"top\"><b>icons</b></td></tr>

<tr><td valign=\"top\"><b>electrical<br>analog</b></td>
   <td valign=\"top\">electrical potential</td>
   <td valign=\"top\">electrical current</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Analog.Interfaces\">Modelica.Electrical.Analog.Interfaces</a>
     <br>Pin, PositivePin, NegativePin</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/ElectricalPins.png\"></td></tr>

<tr><td valign=\"top\"><b>electrical<br>multi-phase</b></td>
   <td colspan=\"3\">vector of electrical pins</td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Electrical.MultiPhase.Interfaces\">Modelica.Electrical.MultiPhase.Interfaces</a>
     <br>Plug, PositivePlug, NegativePlug</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/ElectricalPlugs.png\"></td></tr>

<tr><td valign=\"top\"><b>electrical <br>space phasor</b></td>
   <td valign=\"top\">2 electrical potentials</td>
   <td valign=\"top\">2 electrical currents</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Machines.Interfaces\">Modelica.Electrical.Machines.Interfaces</a>
     <br>SpacePhasor</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/SpacePhasor.png\"></td></tr>

<tr><td valign=\"top\"><b>quasi<br>stationary<br>single phase</b></td>
   <td valign=\"top\">complex electrical potential</td>
   <td valign=\"top\">complex electrical current</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Electrical.QuasiStationary.SinglePhase.Interfaces\">
                                       Modelica.Electrical.QuasiStationary.SinglePhase.Interfaces</a>
     <br>Pin, PositivePin, NegativePin</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/QuasiStationarySinglePhasePins.png\"></td></tr>

<tr><td valign=\"top\"><b>quasi<br>stationary<br>multi-phase</b></td>
   <td colspan=\"3\">vector of quasi stationary single phase pins</td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Electrical.QuasiStationary.MultiPhase.Interfaces\">Modelica.Electrical.QuasiStationary.MultiPhase.Interfaces</a>
     <br>Plug, PositivePlug, NegativePlug</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/QuasiStationaryMultiPhasePlugs.png\"></td></tr>

<tr><td valign=\"top\"><b>electrical <br>digital</b></td>
   <td valign=\"top\">Integer (1..9)</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Digital.Interfaces\">Modelica.Electrical.Digital.Interfaces</a>
     <br>DigitalSignal, DigitalInput, DigitalOutput</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/Digital.png\"></td></tr>

<tr><td valign=\"top\"><b>magnetic<br>flux tubes</b></td>
   <td valign=\"top\">magnetic potential</td>
   <td valign=\"top\">magnetic flux</td>
   <td valign=\"top\"></td>
   <td valign=\"top\">
<a href=\"modelica://Modelica.Magnetic.FluxTubes.Interfaces\">Modelica.Magnetic.FluxTubes.Interfaces</a>
     <br>MagneticPort, PositiveMagneticPort, <br>NegativeMagneticPort</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/MagneticPorts.png\"></td></tr>

<tr><td valign=\"top\"><b>magnetic<br>fundamental<br>wave</b></td>
   <td valign=\"top\">complex magnetic potential</td>
   <td valign=\"top\">complex magnetic flux</td>
   <td valign=\"top\"></td>
   <td valign=\"top\">
<a href=\"modelica://Modelica.Magnetic.FundamentalWave.Interfaces\">Modelica.Magnetic.FundamentalWave.Interfaces</a>
     <br>MagneticPort, PositiveMagneticPort, <br>NegativeMagneticPort</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/FundamentalWavePorts.png\"></td></tr>

<tr><td valign=\"top\"><b>translational</b></td>
   <td valign=\"top\">distance</td>
   <td valign=\"top\">cut-force</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.Translational.Interfaces\">Modelica.Mechanics.Translational.Interfaces</a>
     <br>Flange_a, Flange_b</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/TranslationalFlanges.png\"></td></tr>

<tr><td valign=\"top\"><b>rotational</b></td>
   <td valign=\"top\">angle</td>
   <td valign=\"top\">cut-torque</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.Rotational.Interfaces\">Modelica.Mechanics.Rotational.Interfaces</a>
     <br>Flange_a, Flange_b</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/RotationalFlanges.png\"></td></tr>

<tr><td valign=\"top\"><b>3-dim.<br>mechanics</b></td>
   <td valign=\"top\">position vector<br>
    orientation object</td>
   <td valign=\"top\">cut-force vector<br>
    cut-torque vector</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.MultiBody.Interfaces\">Modelica.Mechanics.MultiBody.Interfaces</a>
     <br>Frame, Frame_a, Frame_b, Frame_resolve</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/MultiBodyFrames.png\"></td></tr>

<tr><td valign=\"top\"><b>simple<br>fluid flow</b></td>
   <td valign=\"top\">pressure<br>
    specific enthalpy</td>
   <td valign=\"top\">mass flow rate<br>
    enthalpy flow rate</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Thermal.FluidHeatFlow.Interfaces\">Modelica.Thermal.FluidHeatFlow.Interfaces</a>
     <br>FlowPort, FlowPort_a, FlowPort_b</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/FluidHeatFlowPorts.png\"></td></tr>

<tr><td valign=\"top\"><b>thermo<br>fluid flow</b></td>
   <td valign=\"top\">pressure</td>
   <td valign=\"top\">mass flow rate</td>
   <td valign=\"top\">specific enthalpy<br>mass fractions</td>
   <td valign=\"top\">
<a href=\"modelica://Modelica.Fluid.Interfaces\">Modelica.Fluid.Interfaces</a>
     <br>FluidPort, FluidPort_a, FluidPort_b</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/FluidPorts.png\"></td></tr>

<tr><td valign=\"top\"><b>heat<br>transfer</b></td>
   <td valign=\"top\">temperature</td>
   <td valign=\"top\">heat flow rate</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Thermal.HeatTransfer.Interfaces\">Modelica.Thermal.HeatTransfer.Interfaces</a>
     <br>HeatPort, HeatPort_a, HeatPort_b</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/ThermalHeatPorts.png\"></td></tr>

<tr><td valign=\"top\"><b>blocks</b></td>
   <td valign=\"top\">
    Real variable<br>
    Integer variable<br>
    Boolean variable</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.Blocks.Interfaces\">Modelica.Blocks.Interfaces</a>
     <br>
      RealSignal, RealInput, RealOutput<br>
      IntegerSignal, IntegerInput, IntegerOutput<br>
      BooleanSignal, BooleanInput, BooleanOutput</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/Signals.png\"></td></tr>

<tr><td valign=\"top\"><b>complex<br>blocks</b></td>
   <td valign=\"top\">
    Complex variable</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.ComplexBlocks.Interfaces\">Modelica.ComplexBlocks.Interfaces</a>
     <br>ComplexSignal, ComplexInput, ComplexOutput</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/ComplexSignals.png\"></td></tr>

<tr><td valign=\"top\"><b>state<br>machine</b></td>
   <td valign=\"top\">Boolean variables<br>
    (occupied, set, <br>
     available, reset)</td>
   <td valign=\"top\"></td>
   <td valign=\"top\"></td>
   <td valign=\"top\"><a href=\"modelica://Modelica.StateGraph.Interfaces\">Modelica.StateGraph.Interfaces</a>
     <br>Step_in, Step_out, Transition_in, Transition_out</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/StateGraphPorts.png\"></td></tr>

<tr><td colspan=\"5\">&nbsp;<br><b>Connectors from other libraries</b></td></tr>

<tr><td valign=\"top\"><b>hydraulic</b></td>
   <td valign=\"top\">pressure</td>
   <td valign=\"top\">volume flow rate</td>
   <td valign=\"top\"></td>
   <td valign=\"top\">HyLibLight.Interfaces
     <br>Port_A, Port_b</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/HydraulicPorts.png\"></td></tr>

<tr><td valign=\"top\"><b>pneumatic</b></td>
   <td valign=\"top\">pressure</td>
   <td valign=\"top\">mass flow rate</td>
   <td valign=\"top\"></td>
   <td valign=\"top\">PneuLibLight.Interfaces
     <br>Port_1, Port_2</td>
   <td valign=\"top\"><img src=\"modelica://Modelica/Resources/Images/UsersGuide/PneumaticPorts.png\"></td></tr>
</table>

<p>
In all domains, usually 2 connectors are defined. The variable declarations
are <b>identical</b>, only the icons are different in order that it is easy
to distinguish connectors of the same domain that are attached at the same
component.
</p>

<h4>Hierarchical Connectors </h4>
<p>
Modelica supports also hierarchical connectors, in a similar way as hierarchical models.
As a result, it is, e.g., possible, to collect elementary connectors together.
For example, an electrical plug consisting of two electrical pins can be defined as:
</p>

<blockquote>
<pre>
<b>connector</b> Plug
   <b>import</b> Modelica.Electrical.Analog.Interfaces;
   Interfaces.PositivePin phase;
   Interfaces.NegativePin ground;
<b>end</b> Plug;
</pre>
</blockquote>

<p>
With one connect(..) equation, either two plugs can be connected
(and therefore implicitly also the phase and ground pins) or a
Pin connector can be directly connected to the phase or ground of
a Plug connector, such as \"connect(resistor.p, plug.phase)\".
</p>

<h4 id=\"ConnectorEquations\">Connector Equations</h4>

<p>
The connector variables listed above have been basically determined
with the following strategy:
</p>

<ol>
<li> State the relevant balance equations and boundary
     conditions of a volume for the particular physical domain.</li>
<li> Simplify the balance equations and boundary conditions
     of (1) by taking the
     limit of an infinitesimal small volume
     (e.g., thermal domain:
      temperatures are identical and heat flow rates
      sum up to zero).
</li>
<li> Use the variables needed for the balance equations
     and boundary conditions of (2)
     in the connector and select appropriate Modelica
     <b>prefixes</b>, so that these equations
     are generated by the Modelica connection semantics.
</li>
</ol>

<p>
The Modelica connection semantics is sketched at hand
of an example: Three connectors c1, c2, c3 with the definition
</p>

<pre>
<b>connector</b> Demo
  Real        p;  // potential variable
  <b>flow</b>   Real f;  // flow variable
  <b>stream</b> Real s;  // stream variable
<b>end</b> Demo;
</pre>

<p>
are connected together with
</p>

<pre>
   <b>connect</b>(c1,c2);
   <b>connect</b>(c1,c3);
</pre>

<p>
then this leads to the following equations:
</p>

<pre>
  // Potential variables are identical
  c1.p = c2.p;
  c1.p = c3.p;

  // The sum of the flow variables is zero
  0 = c1.f + c2.f + c3.f;

  /* The sum of the product of flow variables and upstream stream variables is zero
     (this implicit set of equations is explicitly solved when generating code;
     the \"&lt;undefined&gt;\" parts are defined in such a way that
     inStream(..) is continuous).
  */
  0 = c1.f*(<b>if</b> c1.f > 0 <b>then</b> s_mix <b>else</b> c1.s) +
      c2.f*(<b>if</b> c2.f > 0 <b>then</b> s_mix <b>else</b> c2.s) +
      c3.f*(<b>if</b> c3.f > 0 <b>then</b> s_mix <b>else</b> c3.s);

  <b>inStream</b>(c1.s) = <b>if</b> c1.f > 0 <b>then</b> s_mix <b>else</b> &lt;undefined&gt;;
  <b>inStream</b>(c2.s) = <b>if</b> c2.f > 0 <b>then</b> s_mix <b>else</b> &lt;undefined&gt;;
  <b>inStream</b>(c3.s) = <b>if</b> c3.f > 0 <b>then</b> s_mix <b>else</b> &lt;undefined&gt;;
</pre>

</html>"));
end Connectors;

  package Conventions "Conventions"
    extends Modelica.Icons.Information;
    package Documentation "HTML documentation"
      extends Modelica.Icons.Information;

      package Format "Format"
        extends Modelica.Icons.Information;

        class Cases "Cases"
          extends Modelica.Icons.Information;
          annotation (Documentation(info="<html>

<p>In the Modelica documentation sometimes different cases have to be distinguished. If the case distinction refers to Modelica parameters or variables (Boolean expressions) the comparisons should be written in the style of Modelica code within <code>&lt;code&gt;</code> and <code>&lt;/code&gt;</code>
</p>

<h5>Example 1</h5>

<code>&lt;p&gt;If &lt;code&gt;useCage == true&lt;/code&gt;, a damper cage is considered in the model...&lt;/p&gt;</code>

<p>appears as</p>

<p>If <code>useCage == true</code>, a damper cage is considered in the model...</p>

<p>
For more complex case scenarios a unordered list should be used. In this case only Modelica specific code segments and Boolean expressions.
</p>

<h5>Example 2</h5>

<pre>
&lt;ul&gt;
  &lt;li&gt; If &lt;code&gt;useCage == true&lt;/code&gt;, a damper cage is considered in the model.
       Cage parameters must be specified in this case.&lt;/li&gt;
  &lt;li&gt; If &lt;code&gt;useCage == false&lt;/code&gt;, the damper cage is omitted.&lt;/li&gt;
&lt;/ul&gt;</pre>

<p>appears as</p>

<ul>
  <li> If <code>useCage == true</code>, a damper cage is considered in the model.
       Cage parameters must be specified in this case.</li>
  <li> If <code>useCage == false</code>, the damper cage is omitted.</li>
</ul>

<p>
In a more equation oriented case additional equations or code segments can be added.
</p>

<h5>Example 3</h5>

<pre>
&lt;ul&gt;
  &lt;li&gt;if &lt;code&gt;usePolar == true&lt;/code&gt;, assign magnitude and angle to output &lt;br&gt;
  &lt;!-- insert graphical representation of equations --&gt;
  y[i,1] = sqrt( a[i]^2 + b[i]^2 ) &lt;br&gt;
  y[i,2] = atan2( b[i], a[i] )
  &lt;/li&gt;
  &lt;li&gt;if &lt;code&gt;usePolar == false&lt;/code&gt;, assign cosine and sine to output &lt;br&gt;
  &lt;!-- insert graphical representation of equations --&gt;
  y[i,1] = a[i] &lt;br&gt;
  y[i,2] = b[i]
  &lt;/li&gt;
&lt;/ul&gt;</pre>

<p>appears as</p>

<ul>
  <li>if <code>usePolar == true</code>, assign magnitude and angle to output <br>

  <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Cases/y_i1_polar.png\"
       alt=\"y[i,1] = sqrt( a[i]^2 + b[i]^2 )\"> <br>
  <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Cases/y_i2_polar.png\"
       alt=\"y[i,2] = atan2( b[i], a[i] )\">
  </li>
  <li>if <code>usePolar == false</code>, assign cosine and sine to output <br>
  <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Cases/y_i1_rect.png\"
       alt=\"y[i,1] = a[i]\"> <br>
  <img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Cases/y_i2_rect.png\"
       alt=\" y[i,2] = b[i]\">
  </li>
</ul>

</html>"));
        end Cases;

        class Code "Code"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>
<p>
<a href=\"modelica://Modelica.UsersGuide.Conventions.ModelicaCode\">Modelica code</a> conventions of class and instance names,
parameters and variables are specified separately. In this section it is summarized how to refer to
Modelica code in the HTML documentation.
</p>

<ol>
<li> For constants, parameters and variables in code segments <code>&lt;code&gt;</code> and <code>&lt;/code&gt;</code>
     should to be used, e.g., <br>
     <code><b>parameter</b> Modelica.SIunits.Time tStart \"Start time\"</code></li>
<li> Write multi or single line code segments using <code>&lt;pre&gt;</code> and <code>&lt;/pre&gt;</code>.</li>
<li> Multi line or single line code shall not be indented.</li>
<li> Inline code segments may be typeset with <code>&lt;code&gt;</code> and <code>&lt;/code&gt;</code>.</li>
<li> In code segments use bold to emphasize Modelica keywords.</li>
</ol>

<h5>Example 1</h5>

<pre>
&lt;pre&gt;
&lt;b&gt;connector&lt;/b&gt; Frame
   ...
   &lt;b&gt;flow&lt;/b&gt; SI.Force f[Woehrnschimmel1998] &lt;b&gt;annotation&lt;/b&gt;(unassignedMessage=\"...\");
&lt;b&gt;end&lt;/b&gt; Frame;
&lt;/pre&gt;</pre>

<p>appears as</p>

<pre>
<b>connector</b> Frame
   ...
   <b>flow</b> SI.Force f[Woehrnschimmel1998] <b>annotation</b>(unassignedMessage=\"...\");
<b>end</b> Frame;
</pre>

<h5>Example 2</h5>

<pre>
&lt;pre&gt;
&lt;b&gt;parameter&lt;/b&gt; Modelica.SIunits.Conductance G=1 &quot;Conductance&quot;;
&lt;/pre&gt;
</pre>

<p>appears as</p>

<pre>
<b>parameter</b> Modelica.SIunits.Conductance G=1 &quot;Conductance&quot;;
</pre>
</html>"));
        end Code;

        class Equations "Equations"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>

<p>
In the context of <a href=\"http://www.w3c.org/\">HTML</a> documentation
equations should have a graphical representation in PNG format. For that purpose tool
specific math typing capabilities can be used. Alternatively the LaTeX to HTML translator
<a href=\"http://www.latex2html.org\">LaTeX2HTML</a>, or the
<a href=\"http://www.homeschoolmath.net/worksheets/equation_editor.php\">Online Equation Editor</a>
or <a href=\"http://www.codecogs.com/latex/eqneditor.php\">codecogs</a> can be used.
</p>

<p>
A typical equation, e.g., of a Fourier synthesis, could look like<br>
<img
 src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Equations/fourier.png\"> <br>
or <br>
<img
 src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Equations/sample.png\"
 alt=\"y=a_1+a_2\"><br>
In an <code>alt</code> tag the original equation should be stored, e.g.,</p>
<pre>
&lt;img
&nbsp;src=&quot;modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Equations/sample.png&quot;
&nbsp;alt=&quot;y=a_1+a_2&quot;&gt;
</pre>

<p>
If one wants to refer to particular variables and parameters in the documentation text, either a
graphical representation (PNG file) or italic fonts for regular physical symbols and lower case
<a href=\"http://www.w3.org/TR/html4/sgml/entities.html\">Greek letters</a>
should be used. Full word variables and full word indices should be spelled within &lt;code&gt; and &lt;/code&gt;.
Vector and array indices should be typeset as subscripts using the &lt;sub&gt; and &lt;/sub&gt; tags.
</p>

<p> Examples for such variables and parameters are:
<i>&phi;</i>, <i>&phi;</i><sub>ref</sub>, <i>v<sub>2</sub></i>, <code>useDamperCage</code>.
</p>

<h4>Numbered equations</h4>

<p>For numbering equations a one row table with two columns should be used. The equation number should be placed in the right column:</p>

<pre>
&lt;table border=\"0\" cellspacing=\"10\" cellpadding=\"2\"&gt;
  &lt;tr&gt;
      &lt;td&gt;&lt;img
      src=&quot;modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Equations/sample.png&quot;
      alt=&quot;y=a_1+a_2&quot;&gt; &lt;/td&gt;
      &lt;td&gt;(1)&lt;/td&gt;
  &lt;/tr&gt;
&lt;/table&gt;
</pre>

<p>appears as:</p>

<table border=\"0\" cellspacing=\"10\" cellpadding=\"2\">
  <tr>
    <td><img
         src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Documentation/Format/Equations/sample.png\"
         alt=\"y=a_1+a_2\"></td>
    <td>(1)</td>
  </tr>
</table>

</html>"));
        end Equations;

        class Figures "Figures"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>
<p>
Figures should in particular be included to examples to discuss the problems and results of the respective model. The library developers are yet encouraged to add figures to the documentation of other components to support the understanding of the users of the library.
</p>

<ol>
<li> Figures have to be placed <strong>outside</strong> of paragraphs to be HTML compliant.</li>
<li> Figures need to have <strong>at least</strong> a <code>src</code> and an <code>alt</code> attribute defined to be HTML compliant.</li>
<li> Technical figures should be placed within a table environment. Each technical figure should then also have a caption. The figure caption starts with a capital letter.</li>
<li> Illustration can be embedded without table environment.</li>
</ol>

<h4>Location of files</h4>

The <code>PNG</code> files should be placed in a folder which exactly represents the package structure.

<h5>Example 1</h5>

<p>This example shows how an illustration should be embedded in the Example
<a href=\"modelica://Modelica.Blocks.Examples.PID_Controller\">PID_Controller</a> of the
<a href=\"modelica://Modelica.Blocks\">Blocks</a> package.</p>

<pre>
&lt;img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Images/Blocks/Examples/PID_controller.png\"
     alt=\"PID_controller.png\"&gt;
</pre>

<h5>Example 2</h5>

<p>This is a simple example of a technical figure with caption.</p>

<pre>
&lt;table border=\"0\" cellspacing=\"0\" cellpadding=\"2\"&gt;
  &lt;caption align=\"bottom\"&gt;Caption starts with a capital letter&lt;/caption&gt;
  &lt;tr&gt;
    &lt;td&gt;
      &lt;img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Images/Blocks/Examples/PID_controller.png\"
           alt=\"PID_controller.png\"&gt;
    &lt;/td&gt;
  &lt;/tr&gt;
&lt;/table&gt;
</pre>

<h5>Example 3</h5>

<p>To refer to a certain figure, a figure number may be added. In such case the figure name (Fig.) including the figure enumeration (1,2,...) have to be displayed bold using <code>&lt;strong&gt;</code> and <code>&lt;/strong&gt;</code>.</p>
<p>The figure name and enumeration should look like this: <strong>Fig. 1:</strong></p>
<p>Figures have to be enumerated manually. </p>

<pre>
&lt;table border=\"0\" cellspacing=\"0\" cellpadding=\"2\"&gt;
  &lt;caption align=\"bottom\"&gt;&lt;strong&gt;Fig. 2: &lt;/strong&gt;Caption starts with a capital letter&lt;/caption&gt;
  &lt;tr&gt;
    &lt;td&gt;
      &lt;img src=\"modelica://Modelica/Resources/Images/UsersGuide/Conventions/Images/Blocks/Examples/PID_controller.png\"
           alt=\"PID_controller.png\"&gt;
    &lt;/td&gt;
  &lt;/tr&gt;
&lt;/table&gt;
</pre>

</html>"));
        end Figures;

        class Hyperlinks "Hyperlinks"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>
<ol>
<li> Hyperlinks should always be made when referring to a component or package.</li>
<li> The hyperlink text in between <code>&lt;a href=&quot;...&quot;&gt;</code> and <code>&lt;/a&gt;</code> should include the full main package name.</li>
<li> A link to an external component should include the full name of the package that it is referred to.</li>
<li> Modelica hyperlinks have to use the scheme <code>&quot;modelica://...&quot;</code></li>
<li> For hyperlinks referring to a Modelica component, see Example 1 and 2.</li>
<li> No links to commercial web sites are allowed.</li>
</ol>
<h5>Example 1</h5>
<pre>
&lt;a href=\"modelica://Modelica.Mechanics.MultiBody.UsersGuide.Tutorial.LoopStructures.PlanarLoops\"&gt;
         Modelica.Mechanics.MultiBody.UsersGuide.Tutorial.LoopStructures.PlanarLoops&lt;/a&gt;</pre>
<p>appears as</p>
<a href=\"modelica://Modelica.Mechanics.MultiBody.UsersGuide.Tutorial.LoopStructures.PlanarLoops\">
         Modelica.Mechanics.MultiBody.UsersGuide.Tutorial.LoopStructures.PlanarLoops</a>
<h5>Example 2</h5>
<pre>
&lt;p&gt;
  The feeder cables are connected to an
  &lt;a href=\"modelica://Modelica.Electrical.Machines.BasicMachines.AsynchronousInductionMachines.AIM_SquirrelCage\"&gt;
  induction machine&lt;/a&gt;.
&lt;/p&gt;</pre>
<p>appears as</p>
<p>
  The feeder cables are connected to an
  <a href=\"modelica://Modelica.Electrical.Machines.BasicMachines.AsynchronousInductionMachines.AIM_SquirrelCage\">
  induction machine</a>.
</p>
</html>"));
        end Hyperlinks;

        class Lists "Lists"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>
<p>
Lists have to be placed <strong>outside</strong> of paragraphs to be HTML compliant.
</p>

<ol>
<li> Items of a list shall start with
<ul>
    <li> a capital letter if each item is a full sentence</li>
    <li> a small letter, if only text fragments are used or the list is fragment of a sentence</li>
</ul></li>
</ol>

<h5>Example 1</h5>

<p>This is a simple example of an enumerated (ordered) list</p>

<pre>
&lt;ol&gt;
  &lt;li&gt;item 1&lt;/li&gt;
  &lt;li&gt;item 2&lt;/li&gt;
&lt;/ol&gt;
</pre>
<p>appears as</p>
<ol>
  <li>item 1</li>
  <li>item 2</li>
</ol>

<h5>Example 2</h5>

<p>This is a simple example of an unnumbered list.</p>

<pre>
&lt;ul&gt;
  &lt;li&gt;item 1&lt;/li&gt;
  &lt;li&gt;item 2&lt;/li&gt;
&lt;/ul&gt;
</pre>
<p>appears as</p>
<ul>
  <li>item 1</li>
  <li>item 2</li>
</ul>
</html>"));
        end Lists;

        class References "References"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>

<h4>General</h4>
<ol>
<li> Refer to references by [1], [Andronov1973], etc. by hyperlink and summarize literature in the references subsection of
     <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.References\">Conventions.UsersGuide.References</a>.</li>
<li> There has to be made at least one citation to each reference.</li>
</ol>

<h5>Example</h5>

<pre>
&lt;p&gt;
  More details about sensorless rotor temperature estimation
  can be found in &lt;a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.References\"&gt;[Gao2008]&lt;/a.&gt;
&lt;/p&gt;</pre>
<p>appears as</p>
<p>
  More details about sensorless rotor temperature estimation can be found in <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.References\">[Gao2008]</a>.
</p>

</html>"));
        end References;

        class Tables "Tables"
          extends Modelica.Icons.Information;

          annotation (Documentation(info="<html>
<ol>
<li> Tables should always be typeset with <code>&lt;table&gt;</code> and <code>&lt;/table&gt;</code>,
     not with <code>&lt;pre&gt;</code> and <code>&lt;/pre&gt;</code>.</li>
<li> Tables have to be placed <strong>outside</strong> of paragraphs to be HTML compliant.</li>
<li> Each table must have a table caption.</li>
<li> Table headers and entries start with capital letters.</li>
</ol>

<h5>Example 1</h5>

<p>This is a simple example of a table.</p>

<pre>
&lt;table border=\"1\" cellspacing=\"0\" cellpadding=\"2\"&gt;
  &lt;caption align=\"bottom\"&gt;Caption starts with a capital letter&lt;/caption&gt;
  &lt;tr&gt;
    &lt;th&gt;Head 1&lt;/th&gt;
    &lt;th&gt;Head 2&lt;/th&gt;
  &lt;/tr&gt;
  &lt;tr&gt;
    &lt;td&gt;Entry 1&lt;/td&gt;
    &lt;td&gt;Entry 2&lt;/td&gt;
  &lt;/tr&gt;
  &lt;tr&gt;
    &lt;td&gt;Entry 3&lt;/td&gt;
    &lt;td&gt;Entry 4&lt;/td&gt;
  &lt;/tr&gt;
&lt;/table&gt;
</pre>
<p>appears as</p>
<table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
  <caption align=\"bottom\">Caption starts with a capital letter</caption>
  <tr>
    <th><b>Head 1</b></th>
    <th><b>Head 2</b></th>
  </tr>
  <tr>
    <td>Entry 1</td>
    <td>Entry 2</td>
  </tr>
  <tr>
    <td>Entry 3</td>
    <td>Entry 4</td>
  </tr>
</table>

<h5>Example 2</h5>

<p>In this case of table captions, the table name (Tab.) including the table enumeration (1,2,...)
has to be displayed bold using <code>&lt;b&gt;</code> and <code>&lt;/b&gt;</code>. The table name
and enumeration should look like this: <b>Tab. 1:</b> Tables have to be enumerated manually.</p>

<pre>
&lt;table border=\"1\" cellspacing=\"0\" cellpadding=\"2\"&gt;
  &lt;caption align=\"bottom\"&gt;&lt;b&gt;Tab 2: &lt;/b&gt;Caption starts with a capital letter&lt;/caption&gt;
  &lt;tr&gt;
    &lt;th&gt;Head 1&lt;/th&gt;
    &lt;th&gt;Head 2&lt;/th&gt;
  &lt;/tr&gt;
  &lt;tr&gt;
    &lt;td&gt;Entry 1&lt;/td&gt;
    &lt;td&gt;Entry 2&lt;/td&gt;
  &lt;/tr&gt;
  &lt;tr&gt;
    &lt;td&gt;Entry 3&lt;/td&gt;
    &lt;td&gt;Entry 4&lt;/td&gt;
  &lt;/tr&gt;
&lt;/table&gt;
</pre>
<p>appears as</p>
<table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
  <caption align=\"bottom\"><b>Tab. 2: </b>Caption starts with a capital letter</caption>
  <tr>
    <th>Head 1</th>
    <th>Head 2</th>
  </tr>
  <tr>
    <td>Entry 1</td>
    <td>Entry 2</td>
  </tr>
  <tr>
    <td>Entry 3</td>
    <td>Entry 4</td>
  </tr>
</table>

</html>"));
        end Tables;
        annotation (Documentation(info="<html>

<p>
In this section the format UsersGuide of the HTML documentation are specified.
The <a href=\"modelica://Modelica.UsersGuide.Conventions.Documentation.Structure\">structure</a> of the documentation is specified separately.
</p>

<h4>Paragraphs</h4>

<ol>
<li> In each section the paragraphs should start with <code>&lt;p&gt;</code>
     and terminate with <code>&lt;/p&gt;</code>.</li>
<li> Do not write plain text without putting it in a paragraph.</li>
<li> No artificial line breaks <code>&lt;br&gt;</code> should be added within text paragraphs if possible.
     Use separate paragraphs instead.</li>
<li> After a colon (:) continue with capital letter if new sentence starts;
     for text fragments continue with lower case letter</li>
</ol>

<h4>Emphasis</h4>

<ol>
<li> For setting text in <strong>strong font</strong> (normally interpreted as <strong>boldface</strong>) the tags <code>&lt;strong&gt;</code> and <code>&lt;/strong&gt;</code> have to be used.</li>
<li> For <em>emphasizing</em> text fragments <code>&lt;em&gt;</code> and <code>&lt;/em&gt;</code> has to be used.</li>
<li> Modelica terms such as expandable bus, array, etc. should not be emphasized anyhow.</li>
</ol>

<h4>Capitalization of Text</h4>

<ol>
<li> Table headers and entries should start with capital letters</li>
<li> Table entries should start with lower case letter if only text fragments are used.</li>
<li> Table and figure captions start with a capital letter</li>
</ol>

</html>"));
      end Format;

      class Structure "Structure"
        extends Modelica.Icons.Information;

        annotation (Documentation(info="<html>

<h4>General</h4>

<ol>
<li> In the HTML documentation of any Modelica library, the headings <code>&lt;h1&gt;</code>,
     <code>&lt;h2&gt;</code> and <code>&lt;h3&gt;</code> should not be used, because they are utilized by
     the automatically generated documentation.</li>
<li> The utilized heading format starts with <code>&lt;h4&gt;</code> and terminates with <code>&lt;/h4&gt;</code>, e.g.,
     <code>&lt;h4&gt;Description&lt;/h4&gt;</code> </li>
<li> The  <code>&lt;h4&gt;</code> and  <code>&lt;h5&gt;</code> headings must not be terminated by a colon (:).</li>
<li> For additional structuring <code>&lt;h5&gt;</code> and <code>&lt;/h5&gt;</code> may be used as demonstrated below.</li>
</ol>

<h4>Structure</h4>
<p>
The following parts should be added to the documentation of each component:
</p>

<ol>
<li> General information without additional subsection explains how the class works</li>
<li> <b>Syntax</b> (for functions only): shows syntax of function call with minimum and full input parameters</li>
<li> <b>Implementation</b> (optional): explains how the implementation is made </li>
<li> <b>Limitations</b> (optional): explains the limitations of the component</li>
<li> <b>Notes</b> (optional): if required/useful </li>
<li> <b>Examples</b> (optional): if required/useful </li>
<li> <b>Acknowledgments</b> (optional): if required </li>
<li> <b>See also</b>: shows hyperlinks to related models </li>
<li> <b>Revision history</b> (optional): if required/intended for a package/model, the revision history
        should be placed in <code>annotation(Documentation(revisions=&quot;...&quot;));</code></li>
</ol>

<p>
These sections should appear in the listed order. The only exceptions are hierarchically structured notes and examples as explained in the following.
</p>

<h4>Additional notes and examples</h4>

<p>Some additional notes or examples may require additional <code>&lt;h5&gt;</code> headings. For either notes or examples the following cases may be applied:</p>

<h5>Example 1</h5>
<p>
This is an example of a single note.
</p>

<pre>
&lt;h5&gt;Note&lt;/h5&gt;
&lt;p&gt;This is the note.&lt;/p&gt;
</pre>

<h5>Example 2</h5>
<p>
This is an example of a very simple structure.
</p>

<pre>
&lt;h5&gt;Notes&lt;/h5&gt;
&lt;p&gt;This is the first note.&lt;/p&gt;
&lt;p&gt;This is the second note.&lt;/p&gt;
</pre>

<h5>Example 3</h5>

<p>
This example shows a more complex structure with enumeration.
</p>

<pre>
&lt;h5&gt;Note 1&lt;/h5&gt;
...
&lt;h5&gt;Note 2&lt;/h5&gt;
...
</pre>
<h4>Automatically created documentation</h4>
<p>
For parameters, connectors, as well as inputs and outputs of function automatic documentation is generated by the tool from the quoted comments.
</p>
</html>"));
      end Structure;
      annotation (Documentation(info="<html>
<a href=\"http://www.w3c.org/\">HTML</a> documentation of Modelica classes.
</html>"));
    end Documentation;

    package ModelicaCode "Modelica code"
      extends Modelica.Icons.Information;

       class Format "Format"
         extends Modelica.Icons.Information;

        annotation (Documentation(info="<html>

<ol>
<li> In the <b>icon</b> of a component the instance name is displayed
     (text string <code>%name</code>) in <b>blue color</b>.
     Parameter values, e.g., resistance, mass, gear ratio, are displayed
     in the icon in <b>black color</b> in a smaller font size as the instance name.</li>
<li> Comments and annotations should start with a capital letter, for example: <br>
     <code><b>parameter</b> Real a = 1 \"Arbitrary factor\";</code>.<br>
     For Boolean parameters, the description string should start with \"= true: ..\", for example:<br>
     <code><b>parameter</b> Boolean useHeatPort = false \"= true, if heatPort is enabled\";</code>.</li>
</ol>

</html>"));
       end Format;

      class Naming "Naming convention"
        extends Modelica.Icons.Information;

        annotation (Documentation(info="<html>

<ol>
<li> <b>Class and instance names</b> are usually written in upper and lower case
     letters, e.g., \"ElectricCurrent\". An underscore may be used in names.
     However, it has to be taken into account that the last underscore in a
     name might indicate that the following characters are rendered as a subscript.
     Example: \"pin_a\" may be rendered as \"pin<sub>a</sub>\".</li>

<li> <b>Class names</b> start always with an upper case letter,
     with the exception of functions, that start with a lower case letter.</li>

<li> <b>Instance names</b>, i.e., names of component instances and
     of variables (with the exception of constants),
     start usually with a lower case letter with only
     a few exceptions if this is common sense
     (such as <code>T</code> for a temperature variable).</li>

<li> <b>Constant names</b>, i.e., names of variables declared with the
     \"constant\" prefix, follow the usual naming conventions
     (= upper and lower case letters) and start usually with an
     upper case letter, e.g., UniformGravity, SteadyState.</li>

<li> The two <b>connectors</b> of a domain that have identical declarations
     and different icons are usually distinguished by <code>_a</code>, <code>_b</code>
     or <code>_p</code>, <code>_n</code>, e.g., <code>Flange_a</code>, <code>Flange_b</code>,
     <code>HeatPort_a</code>, <code>HeatPort_b</code>.</li>

<li> A <b>connector class</b> has the instance
     name definition in the diagram layer and not in the icon layer.</li>
</ol>

<h4>Variable names</h4>
<p>In the following table typical variable names are listed. This list should be completed.</p>

<table border=\"1\" cellpadding=\"2\" cellspacing=\"0\" >
   <caption align=\"bottom\">Variables and names</caption>
   <tr>
      <th>Variable</th>
      <th>Quantity</th>
    </tr>
     <tr>
      <td>a</td>
      <td>acceleration</td>
    </tr>
    <tr>
      <td>A</td>
      <td>area</td>
    </tr>
    <tr>
      <td>C</td>
      <td>capacitance</td>
    </tr>
    <tr>
      <td>d</td>
      <td>damping, density, diameter</td>
    </tr>
    <tr>
      <td>dp</td>
      <td>pressureDrop</td>
    </tr>
    <tr>
      <td>e</td>
      <td>specificEntropy</td>
    </tr>
    <tr>
      <td>E</td>
      <td>energy, entropy</td>
    </tr>
    <tr>
      <td>eta</td>
      <td>efficiency</td>
    </tr>
    <tr>
      <td>f</td>
      <td>force, frequency</td>
    </tr>
    <tr>
      <td>G</td>
      <td>conductance</td>
    </tr>
    <tr>
      <td>H</td>
      <td>enthalpy</td>
    </tr>
    <tr>
      <td>h</td>
      <td>height, specificEnthalpy</td>
    </tr>
    <tr>
      <td>HFlow</td>
      <td>enthalpyFlow</td>
    </tr>
    <tr>
      <td>i</td>
      <td>current</td>
    </tr>
    <tr>
      <td>J</td>
      <td>inertia</td>
    </tr>
    <tr>
      <td>l</td>
      <td>length</td>
    </tr>
    <tr>
      <td>L</td>
      <td>Inductance</td>
    </tr>
    <tr>
      <td>m</td>
      <td>mass</td>
    </tr>
    <tr>
      <td>M</td>
      <td>mutualInductance</td>
    </tr>
    <tr>
      <td>mFlow</td>
      <td>massFlow</td>
    </tr>
    <tr>
      <td>P</td>
      <td>power</td>
    </tr>
    <tr>
      <td>p</td>
      <td>pressure</td>
    </tr>
    <tr>
      <td>Q</td>
      <td>heat</td>
    </tr>
    <tr>
      <td>Qflow</td>
      <td>heatFlow</td>
    </tr>
    <tr>
      <td>r</td>
      <td>radius</td>
    </tr>
    <tr>
      <td>R</td>
      <td>radius, resistance</td>
    </tr>
    <tr>
      <td>t</td>
      <td>time</td>
    </tr>
    <tr>
      <td>T</td>
      <td>temperature</td>
    </tr>
    <tr>
      <td>tau</td>
      <td>torque</td>
    </tr>
    <tr>
      <td>U</td>
      <td>internalEnergy</td>
    </tr>
    <tr>
      <td>v</td>
      <td>electricPotential, specificVolume, velocity, voltage</td>
    </tr>
    <tr>
      <td>V</td>
      <td>volume</td>
    </tr>
    <tr>
      <td>w</td>
      <td>angularVelocity</td>
    </tr>
    <tr>
      <td>X</td>
      <td>reactance</td>
    </tr>
    <tr>
      <td>Z</td>
      <td>impedance</td>
    </tr>
</table>
</html>"));
      end Naming;

      annotation (Documentation(info="<html>

<p>In this section the
<a href=\"modelica://Modelica.UsersGuide.Conventions.ModelicaCode.Naming\">naming conventions</a> of class and instance names, parameters and variables are specified. Additionally some
<a href=\"modelica://Modelica.UsersGuide.Conventions.ModelicaCode.Format\">format UsersGuide</a> are stated.</p>

</html>"));
    end ModelicaCode;

    package UsersGuide "User's Guide"
      extends Modelica.Icons.Information;

      class Implementation "Implementation notes"
        extends Modelica.Icons.Information;

        annotation (Documentation(info="<html>
<p>
This class summarizes general information about the implementation which is not stated elsewhere.
</p>
<ol>
<li>The <code>&lt;caption&gt;</code> tag is currently not supported in some tools.</li>
<li>The <code>&amp;sim;</code> symbol (i.e., '&sim;' ) is currently not supported in some tools.</li>
<li>The <code>&amp;prop;</code> symbol (i.e., '&prop;' ) is currently not supported in some tools.</li>
</ol>
</html>"));
      end Implementation;

      class References "References"
        extends Modelica.Icons.References;

        annotation (Documentation(info="<html>

<ol>
<li> Citation formats should be unified according to IEEE Transactions style.</li>
<li> Reference should be formatted as tables with two columns.</li>
</ol>

<p>In the following the reference formats will be explained based on five examples:</p>

<ul>
<li> Journal (or conference) [Gao2008] </li>
<li> Book [Andronov1973]</li>
<li> Master's thesis [Woehrnschimmel1998]</li>
<li> PhD thesis [Farnleitner1999]</li>
<li> Technical report [Marlino2005]</li>
</ul>

<p>The <a href=\"modelica://Modelica.UsersGuide.Conventions.Documentation.Format.References\">citation</a> is also explained.</p>

<h4>Example</h4>

<pre>
&lt;table border=\"0\" cellspacing=\"0\" cellpadding=\"2\"&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;[Gao2008]&lt;/td&gt;
      &lt;td valign=\"top\"&gt;Z. Gao, T. G. Habetler, R. G. Harley, and R. S. Colby,
        &quot;A sensorless  rotor temperature estimator for induction
                 machines based on a current harmonic spectral
                 estimation scheme,&quot;
        &lt;i&gt;IEEE Transactions on Industrial Electronics&lt;/i&gt;,
        vol. 55, no. 1, pp. 407-416, Jan. 2008.&lt;/td&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;[Andronov1973]&lt;/td&gt;
      &lt;td valign=\"top\"&gt;A. Andronov, E. Leontovich, I. Gordon, and A. Maier,
        &lt;i&gt;Theory of  Bifurcations of Dynamic Systems on a plane&lt;/i&gt;,
        1st ed. New York: J. Wiley &amp; Sons, 1973.&lt;/td&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;[Woehrnschimmel1998]&lt;/td&gt;
      &lt;td valign=\"top\"&gt;R. W&ouml;hrnschimmel,
        &quot;Simulation, modeling and fault detection for vector
              controlled induction machines,&quot;
        Master&#39;s thesis, Vienna University of Technology,
        Vienna, Austria, 1998.&lt;/td&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;[Farnleitner1999]&lt;/td&gt;
      &lt;td valign=\"top\"&gt;E. Farnleitner,
        &quot;Computational Fluid dynamics analysis for rotating
              electrical machinery,&quot;
        Ph.D. dissertation, University of Leoben,
        Department  of Applied Mathematics, Leoben, Austria, 1999.&lt;/td&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;[Marlino2005]&lt;/td&gt;
      &lt;td valign=\"top\"&gt;L. D. Marlino,
        &quot;Oak ridge national laboratory annual progress report for the
              power electronics and electric machinery program,&quot;
      Oak Ridge National Laboratory, prepared for the U.S. Department of Energy,
      Tennessee, USA, Tech. Rep. FY2004 Progress Report, January 2005.&lt;/td&gt;
    &lt;/tr&gt;
&lt;/table&gt;</pre>

<p>appears as</p>

<table border=\"0\" cellspacing=\"0\" cellpadding=\"2\">
    <tr>
      <td valign=\"top\">[Gao08]</td>
      <td valign=\"top\">Z. Gao, T. G. Habetler, R. G. Harley, and R. S. Colby,
        &quot;A sensorless  rotor temperature estimator for induction
                 machines based on a current harmonic spectral
                 estimation scheme,&quot;
        <i>IEEE Transactions on Industrial Electronics</i>,
        vol. 55, no. 1, pp. 407-416, Jan. 2008.</td>
    </tr>
    <tr>
      <td valign=\"top\">[Andronov1973]</td>
      <td valign=\"top\">A. Andronov, E. Leontovich, I. Gordon, and A. Maier,
        <i>Theory of  Bifurcations of Dynamic Systems on a plane</i>,
        1st ed. New York: J. Wiley &amp; Sons, 1973.</td>
    </tr>
    <tr>
      <td valign=\"top\">[Woehrnschimmel1998]</td>
      <td valign=\"top\">R. W&ouml;hrnschimmel,
        &quot;Simulation, modeling and fault detection for vector
              controlled induction machines,&quot;
        Master&#39;s thesis, Vienna University of Technology,
        Vienna, Austria, 1998.</td>
    </tr>
    <tr>
      <td valign=\"top\">[Farnleitner1999]</td>
      <td valign=\"top\">E. Farnleitner,
        &quot;Computational Fluid dynamics analysis for rotating
              electrical machinery,&quot;
        Ph.D. dissertation, University of Leoben,
        Department  of Applied Mathematics, Leoben, Austria, 1999.</td>
    </tr>
    <tr>
      <td valign=\"top\">[Marlino2005]</td>
      <td valign=\"top\">L. D. Marlino,
        &quot;Oak ridge national laboratory annual progress report for the
              power electronics and electric machinery program,&quot;
      Oak Ridge National Laboratory, prepared for the U.S. Department of Energy,
      Tennessee, USA, Tech. Rep. FY2004 Progress Report, January 2005.</td>
    </tr>
</table>

</html>"));
      end References;

      class Contact "Contact"
        extends Modelica.Icons.Contact;

        annotation (Documentation(info="<html>

<p>
This class summarizes contact information of the contributing persons.
</p>

<h5>Example</h5>

<pre>
&lt;p&gt;This package is developed and maintained by the following contributors&lt;/p&gt;
  &lt;table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
    &lt;tr&gt;
      &lt;th&gt;&lt;/th&gt;
      &lt;th&gt;Name&lt;/th&gt;
      &lt;th&gt;Affiliation&lt;/th&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;Library officer&lt;/td&gt;
      &lt;td valign=\"top\"&gt;
      &lt;a href=\"mailto:a.haumer@haumer.at\"&gt;A. Haumer&lt;/a&gt;
      &lt;/td&gt;
      &lt;td valign=\"top\"&gt;
        &lt;a href=\"http://www.haumer.at\"&gt;Technical Consulting &amp;amp; Electrical Engineering&lt;/a&gt;&lt;br&gt;
        3423 St.Andrae-Woerdern&lt;br&gt;
        Austria
      &lt;/td&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;Contributor&lt;/td&gt;
      &lt;td valign=\"top\"&gt;
        &lt;a href=\"mailto:dr.christian.kral@gmail.com\"&gt;C. Kral&lt;/a&gt;
      &lt;/td&gt;
      &lt;td valign=\"top\"&gt;
        &lt;a href=\"http://christiankral.net\"&gt;Electric Machines, Drives and Systems&lt;/a&gt;&lt;br&gt;
        1060 Vienna&lt;br&gt;
        Austria
      &lt;/td&gt;
    &lt;/tr&gt;
  &lt;/table&gt;</pre>

<p>appears as</p>

<p>This package is developed and maintained by the following contributors</p>
  <table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
    <tr>
      <th></th>
      <th>Name</th>
      <th>Affiliation</th>
    </tr>
    <tr>
      <td valign=\"top\">Library officer</td>
      <td valign=\"top\">
      <a href=\"mailto:a.haumer@haumer.at\">A. Haumer</a>
      </td>
      <td valign=\"top\">
        <a href=\"http://www.haumer.at\">Technical Consulting &amp; Electrical Engineering</a><br>
        3423 St.Andrae-Woerdern<br>
        Austria
      </td>
    </tr>
    <tr>
      <td valign=\"top\">Contributor</td>
      <td valign=\"top\">
        <a href=\"mailto:dr.christian.kral@gmail.com\">C. Kral</a>
      </td>
      <td valign=\"top\">
        <a href=\"http://christiankral.net\">Electric Machines, Drives and Systems</a><br>
        1060 Vienna<br>
        Austria
      </td>
    </tr>
    <tr>
      <td valign=\"top\">Contributor</td>
      <td valign=\"top\">
        <a href=\"http://www.linkedin.com/in/dietmarw\">D. Winkler</a>
      </td>
      <td valign=\"top\">
        DWE, Norway
      </td>
    </tr>
  </table>

</html>"));
      end Contact;

      class RevisionHistory "Revision History"
        extends Modelica.Icons.ReleaseNotes;

        annotation (Documentation(info="<html>

<ol>
<li> The revision history needs to answer the question:
     What has changed and what are the improvements over the previous versions and revision.</li>
<li> The revision history includes the documentation of the development history of each class and/or package.</li>
<li> Version number, revision number, date, author and comments shall be included.</li>
</ol>

<h5>Example</h5>

<pre>
&lt;table border=\"1\" cellspacing=\"0\" cellpadding=\"2\"&gt;
    &lt;tr&gt;
      &lt;th&gt;Version&lt;/th&gt;
      &lt;th&gt;Revision&lt;/th&gt;
      &lt;th&gt;Date&lt;/th&gt;
      &lt;th&gt;Author&lt;/th&gt;
      &lt;th&gt;Comment&lt;/th&gt;
    &lt;/tr&gt;
    ...
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;1.0.1&lt;/td&gt;
      &lt;td valign=\"top\"&gt;828&lt;/td&gt;
      &lt;td valign=\"top\"&gt;2008-05-26&lt;/td&gt;
      &lt;td valign=\"top\"&gt;A. Haumer&lt;br&gt;C. Kral&lt;/td&gt;
      &lt;td valign=\"top\"&gt;Fixed bug in documentation&lt;/td&gt;
    &lt;/tr&gt;
    &lt;tr&gt;
      &lt;td valign=\"top\"&gt;1.0.0&lt;/td&gt;
      &lt;td valign=\"top\"&gt;821&lt;/td&gt;
      &lt;td valign=\"top\"&gt;2008-05-21&lt;/td&gt;
      &lt;td valign=\"top\"&gt;A. Haumer&lt;/td&gt;
      &lt;td valign=\"top\"&gt;&lt;/td&gt;
    &lt;/tr&gt;
&lt;/table&gt;</pre>

<p>This code appears then as in the \"Revisions\" section below.</p>

</html>",
      revisions="<html>
<table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
    <tr>
      <th>Version</th>
      <th>Revision</th>
      <th>Date</th>
      <th>Author</th>
      <th>Comment</th>
    </tr>
    <tr>
      <td valign=\"top\">1.1.0</td>
      <td valign=\"top\"></td>
      <td valign=\"top\">2010-04-22</td>
      <td valign=\"top\">C. Kral</td>
      <td valign=\"top\">Migrated Conventions to UsersGuide of MSL</td>
    </tr>
    <tr>
      <td valign=\"top\">1.0.5</td>
      <td valign=\"top\">3540</td>
      <td valign=\"top\">2010-03-11</td>
      <td valign=\"top\">D. Winkler</td>
      <td valign=\"top\">Updated image links guide to new 'modelica://' URIs, added contact details</td>
    </tr>
    <tr>
      <td valign=\"top\">1.0.4</td>
      <td valign=\"top\">2960</td>
      <td valign=\"top\">2009-09-28</td>
      <td valign=\"top\">C. Kral</td>
      <td valign=\"top\">Applied new rules for equations as discussed on the 63rd Modelica Design Meeting</td>
    </tr>
    <tr>
      <td valign=\"top\">1.0.3</td>
      <td valign=\"top\">2579</td>
      <td valign=\"top\">2008-05-26</td>
      <td valign=\"top\">D. Winkler</td>
      <td valign=\"top\">Layout fixes and enhancements</td>
    </tr>
    <tr>
      <td valign=\"top\">1.0.1</td>
      <td valign=\"top\">828</td>
      <td valign=\"top\">2008-05-26</td>
      <td valign=\"top\">A. Haumer<br>C. Kral</td>
      <td valign=\"top\">Fixed bug in documentation</td>
    </tr>
    <tr>
      <td valign=\"top\">1.0.0</td>
      <td valign=\"top\">821</td>
      <td valign=\"top\">2008-05-21</td>
      <td valign=\"top\">A. Haumer</td>
      <td valign=\"top\"></td>
    </tr>
</table>
</html>"));
      end RevisionHistory;

    annotation (Documentation(info="<html>
<p>The UsersGuide of each package should consist of the following classes</p>
<ol>
<li> <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.Contact\">Contact</a> information of
     the library officer and the co-authors </li>
<li> Optional <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.Implementation\">Implementation Notes</a> to give general information about the implementation</li>
<li> <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.References\">References</a> for summarizing the literature of the package</li>
<li> <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide.RevisionHistory\">Revision history </a> to summarize the most important changes and improvements of the package</li>
</ol>
</html>"));
    end UsersGuide;
    annotation (DocumentationClass=true,Documentation(info="<html>
<p>A Modelica main package should be compliant with the UsersGuide stated in this documentation:</p>
<ol>
<li> Conventions of the <a href=\"modelica://Modelica.UsersGuide.Conventions.ModelicaCode\">Modelica code</a> </li>
<li> Consistent HTML documentation <a href=\"modelica://Modelica.UsersGuide.Conventions.Documentation\">UsersGuide</a> </li>
<li> Structure to be provided by a main package
<ul>
     <li> <a href=\"modelica://Modelica.UsersGuide.Conventions.UsersGuide\">User's Guide</a></li>
     <li> <b>Examples</b> containing models demonstrating the usage of the library.</li>
     <li> <b>Components</b> -- in case of a complex library a more detailed structure can be established.</li>
     <li> <b>Sensors</b></li>
     <li> <b>Sources</b></li>
     <li> <b>Interfaces</b> containing connectors and partial models.</li>
     <li> <b>Types</b> containing type, enumeration and choice definitions.</li>
</ul></li>
<li> These packages should appear in the listed order.</li>
</ol>
</html>"));
  end Conventions;

class ParameterDefaults "Parameter defaults"
  extends Modelica.Icons.Information;

 annotation (Documentation(info="<html>

<p>
In this section the convention is summarized how default parameters are
handled in the Modelica Standard Library (since version 3.0).
</p>

<p>
Many models in this library have parameter declarations to define
constants of a model that might be changed before simulation starts.
Example:
</p>

<blockquote>
<pre>
<b>model</b> SpringDamper
<b>parameter</b> Real c(final unit=\"N.m/rad\")    = 1e5 \"Spring constant\";
<b>parameter</b> Real d(final unit=\"N.m.s/rad\")  = 0   \"Damping constant\";
<b>parameter</b> Modelica.SIunits.Angle phi_rel0 = 0   \"Unstretched spring angle\";
...
<b>end</b> SpringDamper;
</pre>
</blockquote>

<p>
In Modelica it is possible to define a default value of a parameter in
the parameter declaration. In the example above, this is performed for
all parameters. Providing default values for all parameters can lead to
errors that are difficult to detect, since a modeler may have forgotten
to provide a meaningful value (the model simulates but gives wrong
results due to wrong parameter values). In general the following basic
situations are present:
</p>

<ol>
<li> The parameter value could be anything (e.g., a spring constant or
  a resistance value) and therefore the user should provide a value in
  all cases. A Modelica translator should warn, if no value is provided.
</li>

<li> The parameter value is not changed in &gt; 95 % of the cases
  (e.g., initialization or visualization parameters, or parameter phi_rel0
  in the example above). In this case a default parameter value should be
  provided, in order that the model or function can be conveniently
  used by a modeler.
</li>

<li> A modeler would like to quickly utilize a model, e.g.,
  <ul>
  <li> to automatically check that the model still translates and/or simulates
    (after some changes in the library),</li>
  <li> to make a quick demo of a library by drag-and-drop of components,</li>
  <li> to implement a simple test model in order to get a better understanding
    of the desired component.</li>
  </ul>
  In all these cases, it would be not practical, if the modeler would
  have to provide explicit values for all parameters first.
  </li>
</ol>

<p>
To handle the conflicting goals of (1) and (3), the Modelica Standard Library
uses two approaches to define default parameters, as demonstrated with the
following example:
</p>

<blockquote>
<pre>
<b>model</b> SpringDamper
<b>parameter</b> Real c(final unit=\"N.m/rad\"  , start=1e5) \"Spring constant\";
<b>parameter</b> Real d(final unit=\"N.m.s/rad\", start=  0) \"Damping constant\";
<b>parameter</b> Modelica.SIunits.Angle phi_rel0 = 0       \"Unstretched spring angle\";
...
<b>end</b> SpringDamper;

SpringDamper sp1;              // warning for \"c\" and \"d\"
SpringDamper sp2(c=1e4, d=0);  // fine, no warning
</pre>
</blockquote>

<p>
Both definition forms, using a \"start\" value (for \"c\" and \"d\") and providing
a declaration equation (for \"phi_rel0\"), are valid Modelica and define the value
of the parameter. By convention, it is expected that Modelica translators will
trigger a warning message for parameters that are <b>not</b> defined by a declaration
equation, by a modifier equation or in an initial equation/algorithm section.
A Modelica translator might have options to change this behavior, especially,
that no messages are printed in such cases and/or that an error is triggered
instead of a warning.
</p>

</html>"));
end ParameterDefaults;

class ModelicaLicense2 "Modelica License 2"
  extends Modelica.Icons.Information;

  annotation (Documentation(info="<html>
<head>
    <title>The Modelica License 2</title>
    <style type=\"text/css\">
    *       { font-size: 10pt; font-family: Arial,sans-serif; }
    code    { font-size:  9pt; font-family: Courier,monospace;}
    h6      { font-size: 10pt; font-weight: bold; color: green; }
    h5      { font-size: 11pt; font-weight: bold; color: green; }
    h4      { font-size: 13pt; font-weight: bold; color: green; }
    address {                  font-weight: normal}
    td      { solid #000; vertical-align:top; }
    th      { solid #000; vertical-align:top; font-weight: bold; }
    table   { solid #000; border-collapse: collapse;}
    </style>
</head>
<body lang=\"en-US\">
    <p>All files in this directory (&quot;Modelica&quot;) and in all
    subdirectories, especially all files that build package
    &quot;Modelica&quot; and the contents of the directory
    &quot;Modelica/Resources&quot; are licensed by the <strong>Modelica
    Association</strong> under the &quot;Modelica License&nbsp;2&quot; (with
    exception of the contents of the directory
    &quot;Modelica/Resources/C-Sources&quot;).</p>
    <p style=\"margin-left: 40px;\"><strong>Licensor:</strong><br>
    Modelica Association<br>
    (Ideella F&ouml;reningar 822003-8858 in Link&ouml;ping)<br>
    c/o PELAB, IDA, Link&ouml;pings Universitet<br>
    S-58183 Link&ouml;ping<br>
    Sweden<br>
    email: Board@Modelica.org<br>
    web: <a href=\"https://www.Modelica.org\">https://www.Modelica.org</a></p>
    <p style=\"margin-left: 40px;\"><strong>Copyright notices of the
    files:</strong><br>
    Copyright &copy; 1998-2016, ABB, Austrian Institute of Technology,
    T.&nbsp;B&ouml;drich, DLR, Dassault Syst&egrave;mes AB, Fraunhofer,
    A.&nbsp;Haumer, ITI, Modelon, TU Hamburg-Harburg, Politecnico di Milano,
    XRG Simulation</p>
    <p><a href=\"#The_Modelica_License_2-outline\">The Modelica
    License&nbsp;2</a><br>
    <a href=\"#Frequently_Asked_Questions-outline\">Frequently Asked
    Questions</a><br></p>
    <hr>
    <h4><a name=\"The_Modelica_License_2-outline\" id=
    \"The_Modelica_License_2-outline\"></a>The Modelica License&nbsp;2</h4>
    <p><strong>Preamble.</strong> The goal of this license is that Modelica
    related model libraries, software, images, documents, data files etc. can
    be used freely in the original or a modified form, in open source and in
    commercial environments (as long as the license conditions below are
    fulfilled, in particular sections&nbsp;2c) and 2d). The Original Work is
    provided free of charge and the use is completely at your own risk.
    Developers of free Modelica packages are encouraged to utilize this license
    for their work.</p>
    <p>The Modelica License applies to any Original Work that contains the
    following licensing notice adjacent to the copyright notice(s) for this
    Original Work:</p>
    <p><strong>Licensed by the Modelica Association under the Modelica
    License&nbsp;2</strong></p>
    <p><strong>1. Definitions.</strong></p>
    <ol type=\"a\">
        <li>&quot;License&quot; is this Modelica License.</li>
        <li>&quot;Original Work&quot; is any work of authorship, including
        software, images, documents, data files, that contains the above
        licensing notice or that is packed together with a licensing notice
        referencing it.</li>
        <li>&quot;Licensor&quot; is the provider of the Original Work who has
        placed this licensing notice adjacent to the copyright notice(s) for
        the Original Work. The Original Work is either directly provided by the
        owner of the Original Work, or by a licensee of the owner.</li>
        <li>&quot;Derivative Work&quot; is any modification of the Original
        Work which represents, as a whole, an original work of authorship. For
        the matter of clarity and as examples:
            <ol type=\"a\">
                <li>Derivative Work shall not include work that remains
                separable from the Original Work, as well as merely extracting
                a part of the Original Work without modifying it.</li>
                <li>Derivative Work shall not include (a) fixing of errors
                and/or (b) adding vendor specific Modelica annotations and/or
                (c) using a subset of the classes of a Modelica package, and/or
                (d) using a different representation, e.g., a binary
                representation.</li>
                <li>Derivative Work shall include classes that are copied from
                the Original Work where declarations, equations or the
                documentation are modified.</li>
                <li>Derivative Work shall include executables to simulate the
                models that are generated by a Modelica translator based on the
                Original Work (of a Modelica package).</li>
            </ol>
        </li>
        <li>&quot;Modified Work&quot; is any modification of the Original
        Work with the following exceptions: (a) fixing of errors and/or (b)
        adding vendor specific Modelica annotations and/or (c) using a subset
        of the classes of a Modelica package, and/or (d) using a different
        representation, e.g., a binary representation.</li>
        <li>&quot;Source Code&quot; means the preferred form of the Original
        Work for making modifications to it and all available documentation
        describing how to modify the Original Work.</li>
        <li>&quot;You&quot; means an individual or a legal entity exercising
        rights under, and complying with all of the terms of, this
        License.</li>
        <li>&quot;Modelica package&quot; means any Modelica library that is
        defined with the
        &quot;<code><strong>package</strong>&nbsp;&lt;Name&gt;&nbsp;...&nbsp;<strong>end</strong>&nbsp;&lt;Name&gt;;</code>&quot;
        Modelica language element.</li>
    </ol>
    <p><strong>2. Grant of Copyright License.</strong> Licensor grants You a
    worldwide, royalty-free, non-exclusive, sublicensable license, for the
    duration of the copyright, to do the following:</p>
    <ol type=\"a\">
        <li>
            <p>To reproduce the Original Work in copies, either alone or as
            part of a collection.</p>
        </li>
        <li>
            <p>To create Derivative Works according to Section&nbsp;1d) of this
            License.</p>
        </li>
        <li>
            <p>To distribute or communicate to the public copies of the
            <u>Original Work</u> or a <u>Derivative Work</u> under <u>this
            License</u>. No fee, neither as a copyright-license fee, nor as a
            selling fee for the copy as such may be charged under this License.
            Furthermore, a verbatim copy of this License must be included in
            any copy of the Original Work or a Derivative Work under this
            License.<br>
            For the matter of clarity, it is permitted A) to distribute or
            communicate such copies as part of a (possible commercial)
            collection where other parts are provided under different licenses
            and a license fee is charged for the other parts only and B) to
            charge for mere printing and shipping costs.</p>
        </li>
        <li>
            <p>To distribute or communicate to the public copies of a
            <u>Derivative Work</u>, alternatively to Section&nbsp;2c), under
            <u>any other license</u> of your choice, especially also under a
            license for commercial/proprietary software, as long as You comply
            with Sections&nbsp;3, 4 and 8 below.<br>
            For the matter of clarity, no restrictions regarding fees, either
            as to a copyright-license fee or as to a selling fee for the copy
            as such apply.</p>
        </li>
        <li>
            <p>To perform the Original Work publicly.</p>
        </li>
        <li>
            <p>To display the Original Work publicly.</p>
        </li>
    </ol>
    <p><strong>3. Acceptance.</strong> Any use of the Original Work or a
    Derivative Work, or any action according to either Section&nbsp;2a) to 2f)
    above constitutes Your acceptance of this License.</p>
    <p><strong>4. Designation of Derivative Works and of Modified
    Works.</strong> The identifying designation of Derivative Work and of
    Modified Work must be different to the corresponding identifying
    designation of the Original Work. This means especially that the
    (root-level) name of a Modelica package under this license must be changed
    if the package is modified (besides fixing of errors, adding vendor
    specific Modelica annotations, using a subset of the classes of a Modelica
    package, or using another representation, e.g. a binary
    representation).</p>
    <p><strong>5. Grant of Patent License.</strong> Licensor grants You a
    worldwide, royalty-free, non-exclusive, sublicensable license, under patent
    claims owned by the Licensor or licensed to the Licensor by the owners of
    the Original Work that are embodied in the Original Work as furnished by
    the Licensor, for the duration of the patents, to make, use, sell, offer
    for sale, have made, and import the Original Work and Derivative Works
    under the conditions as given in Section&nbsp;2. For the matter of clarity,
    the license regarding Derivative Works covers patent claims to the extent
    as they are embodied in the Original Work only.</p>
    <p><strong>6. Provision of Source Code.</strong> Licensor agrees to provide
    You with a copy of the Source Code of the Original Work but reserves the
    right to decide freely on the manner of how the Original Work is
    provided.<br>
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;For the matter of clarity, Licensor
    might provide only a binary representation of the Original Work. In that
    case, You may (a) either reproduce the Source Code from the binary
    representation if this is possible (e.g., by performing a copy of an
    encrypted Modelica package, if encryption allows the copy operation) or (b)
    request the Source Code from the Licensor who will provide it to You.</p>
    <p><strong>7. Exclusions from License Grant.</strong> Neither the names of
    Licensor, nor the names of any contributors to the Original Work, nor any
    of their trademarks or service marks, may be used to endorse or promote
    products derived from this Original Work without express prior permission
    of the Licensor. Except as otherwise expressly stated in this License and
    in particular in Sections&nbsp;2 and 5, nothing in this License grants any
    license to Licensor&apos;s trademarks, copyrights, patents, trade secrets or any
    other intellectual property, and no patent license is granted to make, use,
    sell, offer for sale, have made, or import embodiments of any patent
    claims.<br>
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;No license is granted to the trademarks
    of Licensor even if such trademarks are included in the Original Work,
    except as expressly stated in this License. Nothing in this License shall
    be interpreted to prohibit Licensor from licensing under terms different
    from this License any Original Work that Licensor otherwise would have a
    right to license.</p>
    <p><strong>8. Attribution Rights.</strong> You must retain in the Source
    Code of the Original Work and of any Derivative Works that You create, all
    author, copyright, patent, or trademark notices, as well as any descriptive
    text identified therein as an &quot;Attribution Notice&quot;. The same
    applies to the licensing notice of this License in the Original Work. For
    the matter of clarity, &quot;author notice&quot; means the notice that
    identifies the original author(s).<br>
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;You must cause the Source Code for any
    Derivative Works that You create to carry a prominent Attribution Notice
    reasonably calculated to inform recipients that You have modified the
    Original Work.<br>
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;In case the Original Work or Derivative
    Work is not provided in Source Code, the Attribution Notices shall be
    appropriately displayed, e.g., in the documentation of the Derivative
    Work.</p>
    <p><strong>9. Disclaimer of Warranty.<br></strong> <u><strong>The Original
    Work is provided under this License on an &quot;as is&quot; basis and
    without warranty, either express or implied, including, without limitation,
    the warranties of non-infringement, merchantability or fitness for a
    particular purpose. The entire risk as to the quality of the Original Work
    is with You.</strong></u> This disclaimer of warranty constitutes an
    essential part of this License. No license to the Original Work is granted
    by this License except under this disclaimer.</p>
    <p><strong>10. Limitation of Liability.</strong> Under no circumstances and
    under no legal theory, whether in tort (including negligence), contract, or
    otherwise, shall the Licensor, the owner or a licensee of the Original Work
    be liable to anyone for any direct, indirect, general, special, incidental,
    or consequential damages of any character arising as a result of this
    License or the use of the Original Work including, without limitation,
    damages for loss of goodwill, work stoppage, computer failure or
    malfunction, or any and all other commercial damages or losses. This
    limitation of liability shall not apply to the extent applicable law
    prohibits such limitation.</p>
    <p><strong>11. Termination.</strong> This License conditions your rights to
    undertake the activities listed in Section&nbsp;2 and 5, including your
    right to create Derivative Works based upon the Original Work, and doing so
    without observing these terms and conditions is prohibited by copyright law
    and international treaty. Nothing in this License is intended to affect
    copyright exceptions and limitations. This License shall terminate
    immediately and You may no longer exercise any of the rights granted to You
    by this License upon your failure to observe the conditions of this
    license.</p>
    <p><strong>12. Termination for Patent Action.</strong> This License shall
    terminate automatically and You may no longer exercise any of the rights
    granted to You by this License as of the date You commence an action,
    including a cross-claim or counterclaim, against Licensor, any owners of
    the Original Work or any licensee alleging that the Original Work infringes
    a patent. This termination provision shall not apply for an action alleging
    patent infringement through combinations of the Original Work under
    combination with other software or hardware.</p>
    <p><strong>13. Jurisdiction.</strong> Any action or suit relating to this
    License may be brought only in the courts of a jurisdiction wherein the
    Licensor resides and under the laws of that jurisdiction excluding its
    conflict-of-law provisions. The application of the United Nations
    Convention on Contracts for the International Sale of Goods is expressly
    excluded. Any use of the Original Work outside the scope of this License or
    after its termination shall be subject to the requirements and penalties of
    copyright or patent law in the appropriate jurisdiction. This section shall
    survive the termination of this License.</p>
    <p><strong>14. Attorneys&apos; Fees.</strong> In any action to enforce the terms
    of this License or seeking damages relating thereto, the prevailing party
    shall be entitled to recover its costs and expenses, including, without
    limitation, reasonable attorneys&apos; fees and costs incurred in connection
    with such action, including any appeal of such action. This section shall
    survive the termination of this License.</p>
    <p><strong>15. Miscellaneous.</strong></p>
    <ol type=\"a\">
        <li>If any provision of this License is held to be unenforceable, such
        provision shall be reformed only to the extent necessary to make it
        enforceable.</li>
        <li>No verbal ancillary agreements have been made. Changes and
        additions to this License must appear in writing to be valid. This also
        applies to changing the clause pertaining to written form.</li>
        <li>You may use the Original Work in all ways not otherwise restricted
        or conditioned by this License or by law, and Licensor promises not to
        interfere with or be responsible for such uses by You.</li>
    </ol>
    <hr>
    <h5><a name=\"Frequently_Asked_Questions-outline\" id=
    \"Frequently_Asked_Questions-outline\"></a> Frequently Asked Questions</h5>
    <p>This section contains questions/answer to users and/or distributors of
    Modelica packages and/or documents under Modelica License&nbsp;2. Note, the
    answers to the questions below are not a legal interpretation of the
    Modelica License&nbsp;2. In case of a conflict, the language of the license
    shall prevail.</p>
    <h6>Using or Distributing a Modelica <u>Package</u> under the Modelica
    License&nbsp;2</h6>
    <p><strong>What are the main differences to the previous version of the
    Modelica License?</strong></p>
    <ol>
        <li>
            <p>Modelica License&nbsp;1 is unclear whether the licensed Modelica
            package can be distributed under a different license.
            Version&nbsp;2 explicitly allows that &quot;Derivative Work&quot;
            can be distributed under any license of Your choice, see examples
            in Section&nbsp;1d) as to what qualifies as Derivative Work (so,
            version&nbsp;2 is clearer).</p>
        </li>
        <li>
            <p>If You modify a Modelica package under Modelica License&nbsp;2
            (besides fixing of errors, adding vendor specific Modelica
            annotations, using a subset of the classes of a Modelica package,
            or using another representation, e.g., a binary representation),
            you must rename the root-level name of the package for your
            distribution. In version&nbsp;1 you could keep the name (so,
            version&nbsp;2 is more restrictive). The reason of this restriction
            is to reduce the risk that Modelica packages are available that
            have identical names, but different functionality.</p>
        </li>
        <li>
            <p>Modelica License&nbsp;1 states that &quot;It is not allowed to
            charge a fee for the original version or a modified version of the
            software, besides a reasonable fee for distribution and
            support&quot;. Version&nbsp;2 has a similar intention for all
            Original Work under <u>Modelica License&nbsp;2</u> (to remain free
            of charge and open source) but states this more clearly as
            &quot;No fee, neither as a copyright-license fee, nor as a selling
            fee for the copy as such may be charged&quot;. Contrary to
            version&nbsp;1, Modelica License&nbsp;2 has no restrictions on fees
            for Derivative Work that is provided under a different license (so,
            version&nbsp;2 is clearer and has fewer restrictions).</p>
        </li>
        <li>
            <p>Modelica License&nbsp;2 introduces several useful provisions for
            the licensee (articles&nbsp;5, 6, 12), and for the licensor
            (articles&nbsp;7, 12, 13, 14) that have no counter part in
            version&nbsp;1.</p>
        </li>
        <li>
            <p>Modelica License&nbsp;2 can be applied to all type of work,
            including documents, images and data files, contrary to
            version&nbsp;1 that was dedicated for software only (so,
            version&nbsp;2 is more general).</p>
        </li>
    </ol>
    <p><strong>Can I distribute a Modelica package (under Modelica
    License&nbsp;2) as part of my commercial Modelica modeling and simulation
    environment?</strong></p>
    <p>Yes, according to Section&nbsp;2c). However, you are not allowed to
    charge a fee for this part of your environment. Of course, you can charge
    for your part of the environment.</p>
    <p><strong>Can I distribute a Modelica package (under Modelica
    License&nbsp;2) under a different license?</strong></p>
    <p>No. The license of an unmodified Modelica package cannot be changed
    according to Sections&nbsp;2c) and 2d). This means that you cannot
    <u>sell</u> copies of it, any distribution has to be free of charge.</p>
    <p><strong>Can I distribute a Modelica package (under Modelica
    License&nbsp;2) under a different license when I first encrypt the
    package?</strong></p>
    <p>No. Merely encrypting a package does not qualify for Derivative Work and
    therefore the encrypted package has to stay under Modelica
    License&nbsp;2.</p>
    <p><strong>Can I distribute a Modelica package (under Modelica
    License&nbsp;2) under a different license when I first add classes to the
    package?</strong></p>
    <p>No. The package itself remains unmodified, i.e., it is Original Work,
    and therefore the license for this part must remain under Modelica
    License&nbsp;2. The newly added classes can be, however, under a different
    license.</p>
    <p><strong>Can I copy a class out of a Modelica package (under Modelica
    License&nbsp;2) and include it</strong> <u><strong>unmodified</strong></u>
    <strong>in a Modelica package under a</strong>
    <u><strong>commercial/proprietary</strong></u>
    <strong>license?</strong></p>
    <p>No, according to article&nbsp;2c). However, you can include model,
    block, function, package, record and connector classes in your Modelica
    package under <u>Modelica License&nbsp;2</u>. This means that your Modelica
    package could be under a commercial/proprietary license, but one or more
    classes of it are under Modelica License&nbsp;2.<br>
    Note, a &quot;type&quot; class (e.g., type Angle =
    Real(unit=&quot;rad&quot;)) can be copied and included unmodified under a
    commercial/proprietary license (for details, see the next question).</p>
    <p><strong>Can I copy a type class or</strong> <u><strong>part</strong></u>
    <strong>of a model, block, function, record, connector class, out of a
    Modelica package (under Modelica License&nbsp;2) and include it modified or
    unmodified in a Modelica package under a</strong>
    <u><strong>commercial/proprietary</strong></u>
    <strong>license?</strong></p>
    <p>Yes, according to article&nbsp;2d), since this will in the end usually
    qualify as Derivative Work. The reasoning is the following: A type class or
    part of another class (e.g., an equation, a declaration, part of a class
    description) cannot be utilized &quot;by its own&quot;. In order to make
    this &quot;usable&quot;, you have to add additional code in order that
    the class can be utilized. This is therefore usually Derivative Work and
    Derivative Work can be provided under a different license. Note, this only
    holds, if the additional code introduced is sufficient to qualify for
    Derivative Work. Merely, just copying a class and changing, say, one
    character in the documentation of this class would be no Derivative Work
    and therefore the copied code would have to stay under Modelica
    License&nbsp;2.</p>
    <p><strong>Can I copy a class out of a Modelica package (under Modelica
    License&nbsp;2) and include it in</strong> <u><strong>modified</strong></u>
    <strong>form in a</strong> <u><strong>commercial/proprietary</strong></u>
    <strong>Modelica package?</strong></p>
    <p>Yes. If the modification can be seen as a &quot;Derivative Work&quot;,
    you can place it under your commercial/proprietary license. If the
    modification does not qualify as &quot;Derivative Work&quot; (e.g., bug
    fixes, vendor specific annotations), it must remain under Modelica
    License&nbsp;2. This means that your Modelica package could be under a
    commercial/proprietary license, but one or more parts of it are under
    Modelica License&nbsp;2.</p>
    <p><strong>Can I distribute a &quot;save total model&quot; under my
    commercial/proprietary license, even if classes under Modelica
    License&nbsp;2 are included?</strong></p>
    <p>Your classes of the &quot;save total model&quot; can be distributed
    under your commercial/proprietary license, but the classes under Modelica
    License&nbsp;2 must remain under Modelica License&nbsp;2. This means you
    can distribute a &quot;save total model&quot;, but some parts might be
    under Modelica License&nbsp;2.</p>
    <p><strong>Can I distribute a Modelica package (under Modelica
    License&nbsp;2) in encrypted form?</strong></p>
    <p>Yes. Note, if the encryption does not allow &quot;copying&quot; of
    classes (in to unencrypted Modelica source code), you have to send the
    Modelica source code of this package to your customer, if he/she wishes it,
    according to article&nbsp;6.</p>
    <p><strong>Can I distribute an executable under my commercial/proprietary
    license, if the model from which the executable is generated uses models
    from a Modelica package under Modelica License&nbsp;2?</strong></p>
    <p>Yes, according to article&nbsp;2d), since this is seen as Derivative
    Work. The reasoning is the following: An executable allows the simulation
    of a concrete model, whereas models from a Modelica package (without
    pre-processing, translation, tool run-time library) are not able to be
    simulated without tool support. By the processing of the tool and by its
    run-time libraries, significant new functionality is added (a model can be
    simulated whereas previously it could not be simulated) and functionality
    available in the package is removed (e.g., to build up a new model by
    dragging components of the package is no longer possible with the
    executable).</p>
    <p><strong>Is my modification to a Modelica package (under Modelica
    License&nbsp;2) a Derivative Work?</strong></p>
    <p>It is not possible to give a general answer to it. To be regarded as
    &quot;an original work of authorship&quot;, a derivative work must be
    different enough from the original or must contain a substantial amount of
    new material. Making minor changes or additions of little substance to a
    preexisting work will not qualify the work as a new version for such
    purposes.</p>
    <h6>Using or Distributing a Modelica <u>Document</u> under the Modelica
    License&nbsp;2</h6>
    <p>This section is devoted especially for the following applications:</p>
    <ol type=\"a\">
        <li>
            <p>A Modelica tool extracts information out of a Modelica package
            and presents the result in form of a &quot;manual&quot; for this
            package in, e.g., html, doc, or pdf format.</p>
        </li>
        <li>
            <p>The Modelica language specification is a document defining the
            Modelica language. It will be licensed under Modelica
            License&nbsp;2.</p>
        </li>
        <li>
            <p>Someone writes a book about the Modelica language and/or
            Modelica packages and uses information which is available in the
            Modelica language specification and/or the corresponding Modelica
            package.</p>
        </li>
    </ol>
    <p><strong>Can I sell a manual that was basically derived by extracting
    information automatically from a Modelica package under Modelica
    License&nbsp;2 (e.g., a &quot;reference guide&quot; of the Modelica
    Standard Library)?</strong></p>
    <p>Yes. Extracting information from a Modelica package, and providing it in
    a human readable, suitable format, like html, doc or pdf format, where the
    content is significantly modified (e.g. tables with interface information
    are constructed from the declarations of the public variables) qualifies as
    Derivative Work and there are no restrictions to charge a fee for
    Derivative Work under alternative&nbsp;2d).</p>
    <p><strong>Can I copy a text passage out of a Modelica document (under
    Modelica License&nbsp;2) and use it</strong>
    <u><strong>unmodified</strong></u> <strong>in my document (e.g. the
    Modelica syntax description in the Modelica Specification)?</strong></p>
    <p>Yes. In case you distribute your document, the copied parts are still
    under Modelica License&nbsp;2 and you are not allowed to charge a license
    fee for this part. You can, of course, charge a fee for the rest of your
    document.</p>
    <p><strong>Can I copy a text passage out of a Modelica document (under
    Modelica License&nbsp;2) and use it in</strong>
    <u><strong>modified</strong></u> <strong>form in my document?</strong></p>
    <p>Yes, the creation of Derivative Works is allowed. In case the content is
    significantly modified this qualifies as Derivative Work and there are no
    restrictions to charge a fee for Derivative Work under
    alternative&nbsp;2d).</p>
    <p><strong>Can I sell a printed version of a Modelica document (under
    Modelica License&nbsp;2), e.g., the Modelica Language
    Specification?</strong></p>
    <p>No, if you are not the copyright-holder, since article&nbsp;2c) does not
    allow a selling fee for a (in this case physical) copy. However, mere
    printing and shipping costs may be recovered.</p>
</body>
</html>"));
end ModelicaLicense2;

package ReleaseNotes "Release notes"
class VersionManagement "Version Management"
  extends Modelica.Icons.ReleaseNotes;

      annotation (Documentation(info="<html>
<p>
Maintenance of the Modelica Standard Library is performed with
three branches on the subversion server of the Modelica Association
(<a href=\"https://svn.modelica.org/projects/Modelica\">https://svn.modelica.org/projects/Modelica</a>):
</p>

<h4>Released branch</h4>
<p>
Example: \"/tags/v3.0.1/Modelica\"
</p>

<p>
This branch contains the released Modelica versions (e.g., version 3.0.1),
where all available test cases and compatibility checks with other Modelica
libraries have been performed on the respective release. This version is
usually shipped with a Modelica modeling and simulation environment and
utilized by a Modelica user.
</p>

<h4>Development branch</h4>
<p>
Example: \"/trunk/Modelica\"
</p>

<p>
This branch contains the actual development version, i.e., all bug fixes
and new features based on the last Modelica release.
New features should have been tested before including them.
However, the exhaustive tests for a new version are (usually) not performed.
This version is usually only be used by the developers of the
Modelica Standard Library and is not utilized by Modelica users.
</p>

<h4>Maintenance branch</h4>
<p>
Example: \"/branches/maintenance/3.0.1/Modelica\"
</p>

<p>
This branch contains the released Modelica version (e.g., version 3.0.1)
where all bug fixes since this release date are included (up to a  new release,
when becoming available; i.e., after a new release, the previous maintenance
versions are no longer changed).
These bug fixes might be not yet tested with all test cases or with
other Modelica libraries. The goal is that a vendor may take this version at
any time for a new release of its software, in order to incorporate the latest
bug fixes, without changing the version number of the Modelica Standard Library.
</p>

<p>
Incorporation of bug fixes (subversion \"commit\") shall be performed in the following way:
</p>

<ul>
<li> One person is fixing the bug and another person is checking whether the
         fix is fine.</li>
<li> It is up to the library developer, whether he opens a new branch for
         testing and then merges it with the \"head\" maintenance branch or not.</li>
<li> Every change to the maintenance branch has to be done at the development
         branch (see above) as well. One exception are pure changes to the
         \"versionBuild\" annotation as these have no meaning in the development trunk.</li>
<li> Every change to the maintenance branch requires introducing a
         description of the bug fix under
         Modelica.UsersGuide.ReleaseNotes.Version_&lt;release-number&gt;_BugFixes.</li>
<li> Annotations \"version\" and \"versionDate\" must <u>not</u> be changed in a maintenance release.</li>
<li> Every change to the maintenance branch requires changing the \"versionBuild\" number (incrementing it by one),
     as well as the \"dateModified\" field.<br>
     Example:
         <pre>  annotation(version      = \"3.1\",
             versionDate  = \"2009-06-22\",
             versionBuild = 3,
             dateModified = \"2009-08-28 07:40:19Z\",
             revisionId   = \"$I&#8203;d::                                       $\")</pre>
     The \"revisionId\" field is a bit special though. If written like in the example above it will be automatically
     expanded to:
        <pre>             revisionId   = \"$I&#8203;d:: package.mo 2879 2009-08-28 07:40:19Z #$\"</pre>
     by the subversion checkout procedure.</li>
<li> If time does not permit, a vendor makes the bug fix in its local version
         and then has to include it in the maintenance version. It would be best to make these
         changes at a new branch in order to get a unique release number.</li>
</ul>

<p>
A valid \"commit\" to the maintenance branch may contain one or
more of the following changes.
</p>

<ul>
<li> Correcting an equation.</li>
<li> Correcting attributes quantity/unit/defaultUnit in a declaration.</li>
<li> Improving/fixing the documentation.</li>
<li> Introducing a new name in the public section of a class
         (model, package, ...) or in any section of a partial class is <b>not</b> allowed.
         Since otherwise, a user might use this new name and when storing its model
         and loading it with an older build-version, an error would occur.</li>
<li> Introducing a new name in the protected section of a non-partial
         class should only be done if absolutely necessary to fix a bug.
         The problem is that this might be non-backward compatible,
         because a user might already extend from this class and already using the same name.</li>
</ul>
</html>"));
end VersionManagement;

class Version_3_2_2 "Version 3.2.2 (April 3, 2016)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>
<p>
Version 3.2.2 is backward compatible to version 3.2.1, that is models developed with
versions 3.0, 3.0.1, 3.1, 3.2, or 3.2.1 will work without any changes also with version 3.2.2
(with exception of the, usually uncritical, non-backwards compatible changes listed below with regards to
external object libraries, and one bug fix introduced in 3.2.1 Build.3 for non-circular pipes
that can be non-backwards compatible if a user constructed a new pipe model based on
Modelica.Fluid.Pipes.BaseClasses.WallFriction.PartialWallFriction, see details below).
</p>

<ul>
<li> This version of the Modelica package is <b>fully compatible</b> to
     Modelica Specification <b>3.2 revision 2</b>.<br>&nbsp;
     </li>

<li> About <b>240</b> tickets have been fixed in this release and the previous maintenance releases:
     <ul>
     <li> <b>Version 3.2.1 Build.3</b> (July 30, 2015) with respect to 3.2.1 Build.2 (August 14, 2013):<br>
          About <a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.1/ResolvedTracTickets-build-3.html\">103 tickets</a>
          have been fixed for this maintenance release.<br>&nbsp; </li>

     <li> <b>Version 3.2.1 Build.4</b> (September 30, 2015) with respect to 3.2.1 Build.3 (July 30, 2015):
          <ul>
            <li> About <a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.1/ResolvedTracTickets-build-4.html\">10 tickets</a>
                 have been fixed for this maintenance release. Critical tickets:</li>

            <li> Ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1768\">1768</a>
                 fixes an issue with block <a href=\"modelica://Modelica.Blocks.Sources.CombiTimeTable\">CombiTimeTable</a>
                 (wrong output when using fixed time step integrator with time step greater than table resolution).</li>

            <li> Ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1758\">1758</a>
                 states that simulation of
                 <a href=\"modelica://Modelica.Fluid.Examples.HeatingSystem\">Modelica.Fluid.Examples.HeatingSystem</a>
                 fails in Dymola 2016 if option \"pedantic mode for checking Modelica semantics\" is set.
                 This issue was not fixed in the library due to the following reasons:<br>
                 The Modelica.Fluid library uses a particular pattern to define some parameters resulting
                 in a cyclic dependency of parameters if only incident information is taken into account.
                 According to Modelica Specification 3.2 revision 2 this is not allowed
                 (and therefore Dymola 2016 correctly reports errors if the pedantic flag is set).
                 In ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1320\">1320</a>
                 this issue was resolved for Modelica Specification 3.3 revision 1 by allowing
                 cyclic parameter definitions if the cycles disappear when evaluating parameters
                 that have annotation Evaluate=true. Modelica.Fluid is correct with respect
                 to Modelica Specification 3.3 revision 1.
                 Changing the Modelica.Fluid library for 3.2.1 build.4 so that no cyclic parameter dependencies
                 would be present anymore would (a) result in a non-backwards compatible
                 change and (b) make the usage of Modelica.Fluid less convenient. For this
                 reason Modelica.Fluid is not changed. (Practically, this means for example that
                 the pedantic flag in Dymola 2016 needs to be switched off, when using the
                 Modelica.Fluid library in version 3.2.1 build 4 and any previous version).</li>

            <li> In ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1757\">1757</a> it is (correctly) stated
                 that the example model <a href=\"modelica://Modelica.Media.Air.MoistAir.PsychrometricData\">PsychrometricData</a>
                 was moved to another location and that this is a non-backwards compatible change.
                 This non-backwards compatible change is accepted, because it fixes a circular depedency (a model references
                 a package in which it resides), for details see ticket
                 <a href=\"https://trac.modelica.org/Modelica/ticket/1679\">1679</a>.
                 Fixing this ticket is seen as of much higher priority, as the small drawback that
                 an example model is moved (and the probability is very high that this moved model is not
                 used in any user model).<br>&nbsp;
                </li>
          </ul>
     </li>
     <li> <b>Version 3.2.2 Build.2</b> (March 16, 2016) with respect to 3.2.1 Build.4 (September 30, 2015):<br>
          About <a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.2/ResolvedTracTickets.html\">130 tickets</a>
          have been fixed for this release.<br>
          The ModelicaStandardTables object library (.lib, .dll, .a, .so, depending on tool) has
          been split into the libraries <b>ModelicaStandardTables</b>, <b>ModelicaMatIO</b>, <b>zlib</b> and the new
          object library <b>ModelicaIO</b> has been added.<br>
          For a <b>tool vendor</b> this can be a non-backwards compatible change if the same object libraries have been used in the past
          for different releases of package Modelica.
          In <a href=\"modelica://Modelica/Resources/C-sources/_readme.txt\">Resources/C-sources/_readme.txt</a>
          the issue is explained in detail and how to resolve it.
          For a <b>user</b> this might be a non-backwards compatible change if he/she implemented an
          own external C interface function to one of the functions in the ModelicaStandardTables,
          ModelicaMatIO or zlib libraries. In this case, the library annotations to these functions need to be
          adapted.<br>&nbsp;</li>
     </ul>
</li>
<li> In version 3.2.1 Build.3 a new argument crossArea was introduced in the functions of
Modelica.Fluid.Pipes.BaseClasses.WallFriction.PartialWallFriction to fix a subtle bug for the
calculation of pipe friction for non-circular pipes, see <a href=\"https://trac.modelica.org/Modelica/ticket/1601\">#1601</a>
and <a href=\"https://trac.modelica.org/Modelica/ticket/1656\">#1656</a>.
If a user utilized a pipe model of Modelica.Fluid.Pipes, this does not matter because the pipe models have been
improved in a fully backwards compatible way. However, if the user constructed an own pipe model based on
the partial package PartialWallFriction and calls the functions defined in PartialWallFriction with
positional (and not named) arguments, then a unit warning or error will occur (depending on the tool
and tool-specific settings) because the new argument crossArea has unit [m2] and the previous
argument at this place, roughness, has unit [m]. If the warning is ignored, the simulation result
will be wrong, because the crossArea is used as roughness. The user needs to fix this by
adapting his/her pipe model so that the crossArea is used in the function calls,
or by using named function arguments.
</li>
</ul>

<p>
The exact difference between package Modelica version 3.2.2 and version 3.2.1 is
summarized in the following two comparison tables:
</p>
<ul>
<li><a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.2/Differences322To321Build4.html\">Difference 3.2.2 to 3.2.1 Build 4</a>, </li>
<li><a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.2/Differences321Build4toBuild2.html\">Difference 3.2.1 Build 4 to 3.2.1 Build 2</a>.</li>
</ul>

<p>
This release of package Modelica, and the accompanying ModelicaTest, has been tested with the
following tools (the tools are listed alphabetically. At the time of the test, some of the
tools might not yet supported the complete Modelica package. For more details of the tests
see <a href=\"https://trac.modelica.org/Modelica/ticket/1867\">#1867</a>):
</p>

<ul>
<li> <b>Dymola 2017 Beta.1</b> (Windows 64 bit, \"Check\" with pedantic flag, that is checking strict
     Modelica compliance, and \"Check with Simulation\").<br>
     <a href=\"https://trac.modelica.org/Modelica/ticket/1924\">#1924</a>:
     Regression testing of 3.2.2+build.0-beta.2 using Dymola 2017 Dev 4 with respect to
     3.2.1+build.4 reference files<br>
     <a href=\"https://trac.modelica.org/Modelica/ticket/1949\">#1949</a>:
     Regression testing of 3.2.2+build.0-beta.3 using Dymola 2017 Beta 1 with respect to
     3.2.1+build.4 reference files</li>
<li> <b>LMS Imagine.Lab Amesim 14.2</b> and <b>LMS Imagine.Lab Amesim 15 (development build)</b>.
     No previously unreported regressions have been detected.</li>
<li> <b>Maplesim Parser</b></li>
<li> <b>OpenModelica 1.9.4 Beta.2</b> (Windows, Linux, Mac)<br>
     <a href=\"https://test.openmodelica.org/hudson/job/MSL_trunk_Compilation/\">Compilation</a> of models of 3.2.2.<br>
     <a href=\"https://test.openmodelica.org/hudson/job/MSL_trunk_cpp_Simulation/\">Simulation</a> of models of 3.2.2.<br>
     <a href=\"https://test.openmodelica.org/libraries/MSL_trunk/BuildModelRecursive.html\">Regression testing</a> of 3.2.2 using OpenModelica 1.9.4 with respect
     to 3.2.1+build.4 reference files.</li>
     </li>
</ul>

<p>
The following Modelica packages have been tested that they work together with this release of package Modelica
(alphabetical list):
</p>

<ul>
<li>AirConditioning Library 1.12 (Modelon)</li>
<li>Buildings 2.1.0 (LBNL)</li>
<li>Electric Power Library 2.2.3 (Modelon)</li>
<li>Engine Dynamics Library 1.2.5 (Modelon)</li>
<li>FlexibleBodies 2.2 (DLR)</li>
<li>FlightDynamics 1.0.1 (DLR)</li>
<li>FluidDissipation 1.1.8 (XRG Simulation)</li>
<li>Fuel Cell Library 1.3.3 (Modelon)</li>
<li>Heat Exchanger Library 1.4.1 (Modelon)</li>
<li>Human Comfort Library 2.1.0 (XRG Simulation)</li>
<li>HVAC Library 2.1.0 (XRG Simulation)</li>
<li>Hydraulics Library 4.4 (Modelon)</li>
<li>Hydronics Library 2.1.0 (XRG Simulation)</li>
<li>Hydro Power Library 2.6 (Modelon)</li>
<li>Liquid Cooling Library 1.5 (Modelon)</li>
<li>Modelica_Synchronous 0.92.1</li>
<li>Modelica_LinearSystems2 2.3.4 </li>
<li>Modelica_StateGraph2 2.0.3</li>
<li>Optimization 2.2.2 (DLR)</li>
<li>PowerTrain 2.4.0 (DLR)</li>
<li>Pneumatics Library 2.0 (Modelon)</li>
<li>Thermal Power Library 1.12 (Modelon)</li>
<li>Vapor Cycle Library 1.3 (Modelon)</li>
<li>Vehicle Dynamics Library 2.3 (Modelon)</li>
</ul>


<p><br>
The following <b style=\"color:blue\">new libraries</b> have been added:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Electrical.PowerConverters\">Modelica.Electrical.PowerConverters</a></td>
    <td valign=\"top\">
    This library offers models for rectifiers, inverters and DC/DC-converters.<br>
    (This library was developed by Christian Kral and Anton Haumer).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Magnetic.QuasiStatic.FundamentalWave\">Modelica.Magnetic.QuasiStatic.FundamentalWave</a></td>
    <td valign=\"top\">
    This library provides quasi-static models of multiphase machines (induction machines, synchronous machines) in parallel (with the same parameters but different electric connectors)
    to the transient models in <a href=\"modelica://Modelica.Magnetic.FundamentalWave\">Modelica.Magnetic.FundamentalWave</a>.<br>
    Quasistatic means that electric transients are neglected, voltages and currents are supposed to be sinusoidal. Mechanical and thermal transients are taken into account.<br>
    This library is especially useful in combination with the <a href=\"modelica://Modelica.Electrical.QuasiStationary\">Modelica.Electrical.QuasiStationary</a>
    library in order to build up very fast simulations of electrical circuits with sinusoidal currents and voltages.<br>
    (This library was developed by Christian Kral and Anton Haumer).
    </td></tr>

<tr><td valign=\"top\">Sublibraries of <a href=\"modelica://Modelica.Magnetic.FluxTubes\">Modelica.Magnetic.FluxTubes</a></td>
    <td valign=\"top\">
   New elements for modeling ferromagnetic (static) and eddy current (dynamic) hysteresis effects and permanent magnets have been added.
   The FluxTubes.Material package is also extended to provide hysteresis data for several magnetic materials. These data is partly based on own measurements.
   For modeling of ferromagnetic hysteresis, two different hystereses models have been implemented: The simple Tellinen model and the considerably
   more detailed Preisach hysteresis model. The following packages have been added:
  <ul>
  <li><a href=\"Modelica.Magnetic.FluxTubes.UsersGuide.Hysteresis\">FluxTubes.UsersGuide.Hysteresis</a></li>
  <li><a href=\"Modelica.Magnetic.FluxTubes.Examples.Hysteresis\">Fluxtubes.Examples.Hysteresis</a></li>
  <li><a href=\"Modelica.Magnetic.FluxTubes.Shapes.HysteresisAndMagnets\">FluxTubes.Shapes.HysteresisAndMagnets</a></li>
  <li><a href=\"Modelica.Magnetic.FluxTubes.Material.HysteresisEverettParameter\">FluxTubes.Material.HysteresisEverettParameter</a></li>
  <li><a href=\"Modelica.Magnetic.FluxTubes.Material.HysteresisTableData\">FluxTubes.Material.HysteresisTableData</a></li>
  </ul>
    (These extensions have been developed by Johannes Ziske and Thomas B&ouml;drich as part of the <a href=\"http://www.cleansky.eu/\">Clean Sky</a> JTI project;
     project number: 296369; Theme:
   <a href=\"http://cordis.europa.eu/project/rcn/101194_en.html\">JTI-CS-2011-1-SGO-02-026</a>;
   MOMOLIB - Modelica Model Library Development for Media, Magnetic Systems and Wavelets.
     The partial financial support by the European Union for this development is highly appreciated.).
    </td></tr>

<tr><td valign=\"top\">Sublibraries for <b>noise</b> modeling</td>
    <td valign=\"top\">
   Several new sublibraries have been added allowing the modeling of reproducible noise.
   The most important new sublibraries are (for more details see below):
  <ul>
  <li><a href=\"modelica://Modelica.Blocks.Noise\">Modelica.Blocks.Noise</a></li>
  <li><a href=\"modelica://Modelica.Math.Random\">Modelica.Math.Random</a></li>
  <li><a href=\"modelica://Modelica.Math.Distributions\">Modelica.Math.Distributions</a></li>
  <li><a href=\"modelica://Modelica.Math.Special\">Modelica.Math.Special</a></li>
  </ul>
  (These extensions have been developed by  Andreas Kl&ouml;ckner, Frans van der Linden, Dirk Zimmer, and Martin Otter from
  DLR Institute of System Dynamics and Control).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Utilities\">Modelica.Utilities</a> functions for <b>matrix read/write</b></td>
    <td valign=\"top\">
   New functions are provided in the <a href=\"modelica://Modelica.Utilities.Streams\">Modelica.Utilities.Streams</a>
   sublibrary to write matrices in MATLAB MAT format on file and read matrices in this format from file.
   The MATLAB MAT formats v4, v6, v7 and v7.3 (in case the tool supports HDF) are supported by these functions.
   Additionally, example models are provided under
   <a href=\"modelica://Modelica.Utilities.Examples\">Modelica.Utilities.Examples</a>
   to demonstrate the usage of these functions in models. For more details see below.<br>
   (These extensions have been developed by Thomas Beutlich from ITI GmbH).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Math\">Modelica.Math</a> sublibrary for <b>FFT</b></td>
    <td valign=\"top\">
   The new sublibrary <a href=\"modelica://Modelica.Math.FastFourierTransform\">FastFourierTransform</a>
   provides utility and convenience functions to compute the Fast Fourier Transform (FFT).
   Additionally two examples are present to demonstrate how to compute an FFT during continuous-time
   simulation and store the result on file. For more details see below.<br>
  (These extensions have been developed by Martin Kuhn and Martin Otter from
   DLR Institute of System Dynamics and Control).
    </td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries:<br>
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.Blocks.Examples</b></td></tr>
<tr><td valign=\"top\" width=\"150\">NoiseExamples</td>
    <td valign=\"top\"> Several examples to demonstrate the usage of the blocks in the
                      new sublibrary Blocks.Noise.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Interfaces</b></td></tr>
<tr><td valign=\"top\" width=\"150\">PartialNoise</td>
    <td valign=\"top\"> Partial noise generator (base class of the noise generators in Blocks.Noise)</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Math</b></td></tr>
<tr><td valign=\"top\" width=\"150\">ContinuousMean</td>
    <td valign=\"top\"> Calculates the empirical expectation (mean) value of its input signal</td></tr>
<tr><td valign=\"top\" width=\"150\">Variance</td>
    <td valign=\"top\"> Calculates the empirical variance of its input signal</td></tr>
<tr><td valign=\"top\" width=\"150\">StandardDeviation</td>
    <td valign=\"top\"> Calculates the empirical standard deviation of its input signal</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Noise</b></td></tr>
<tr><td valign=\"top\" width=\"150\">GlobalSeed</td>
    <td valign=\"top\"> Defines global settings for the blocks of sublibrary Noise,
                      especially a global seed value is defined</td></tr>
<tr><td valign=\"top\" width=\"150\">UniformNoise</td>
    <td valign=\"top\"> Noise generator with uniform distribution</td></tr>
<tr><td valign=\"top\" width=\"150\">NormalNoise</td>
    <td valign=\"top\"> Noise generator with normal distribution</td></tr>
<tr><td valign=\"top\" width=\"150\">TruncatedNormalNoise</td>
    <td valign=\"top\"> Noise generator with truncated normal distribution</td></tr>
<tr><td valign=\"top\" width=\"150\">BandLimitedWhiteNoise</td>
    <td valign=\"top\"> Noise generator to produce band-limited white noise with normal distribution</td></tr>

<tr><td colspan=\"2\"><b>Modelica.ComplexBlocks.Examples</b></td></tr>
<tr><td valign=\"top\" width=\"150\">ShowTransferFunction</td>
    <td valign=\"top\"> Example to demonstrate the usage of the block TransferFunction.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.ComplexBlocks.ComplexMath</b></td></tr>
<tr><td valign=\"top\" width=\"150\">TransferFunction</td>
    <td valign=\"top\"> This block allows to define a complex transfer function (depending on frequency input w) to obtain the complex output y.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.ComplexBlocks.Sources</b></td></tr>
<tr><td valign=\"top\" width=\"150\">LogFrequencySweep</td>
    <td valign=\"top\"> The logarithm of w performs a linear ramp from log10(wMin) to log10(wMax), the output is the decimal power of this logarithmic ramp.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.Examples.Utilities.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">SpringDamperNoRelativeStates</td>
    <td valign=\"top\">Introduced to fix ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1375\">1375</a></td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.Components.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">ElastoBacklash2</td>
    <td valign=\"top\">Alternative model of backlash. The difference to the existing ElastoBacklash
    component is that an event is generated when contact occurs and that the contact torque
    changes discontinuously in this case. For some user models, this variant of a backlash model
    leads to significantly faster simulations.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Examples.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">NonCircularPipes</td>
    <td valign=\"top\">Introduced to check the fix of ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1601\">1681</a></td></tr>

<tr><td colspan=\"2\"><b>Modelica.Media.Examples.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">PsychrometricData</td>
    <td valign=\"top\">Introduced to fix ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1679\">1679</a></td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">balanceABC</td>
    <td valign=\"top\"> Return a balanced form of a system [A,B;C,0]
                      to improve its condition by a state transformation</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Random.Generators.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">Xorshift64star</td>
    <td valign=\"top\"> Random number generator xorshift64*</td></tr>
<tr><td valign=\"top\" width=\"150\">Xorshift128plus </td>
    <td valign=\"top\"> Random number generator xorshift128+</td></tr>
<tr><td valign=\"top\" width=\"150\">Xorshift1024star</td>
    <td valign=\"top\"> Random number generator xorshift1024*</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Random.Utilities.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">initialStateWithXorshift64star</td>
    <td valign=\"top\"> Return an initial state vector for a random number generator (based on xorshift64star algorithm)</td></tr>
<tr><td valign=\"top\" width=\"150\">automaticGlobalSeed </td>
    <td valign=\"top\"> Creates an automatic integer seed from the current time and process id (= impure function)</td></tr>
<tr><td valign=\"top\" width=\"150\">initializeImpureRandom </td>
    <td valign=\"top\"> Initializes the internal state of the impure random number generator</td></tr>
<tr><td valign=\"top\" width=\"150\">impureRandom</td>
    <td valign=\"top\"> Impure random number generator (with hidden state vector)</td></tr>
<tr><td valign=\"top\" width=\"150\">impureRandomInteger </td>
    <td valign=\"top\"> Impure random number generator for integer values (with hidden state vector)</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Distributions.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">Uniform</td>
    <td valign=\"top\"> Library of uniform distribution functions (functions: density, cumulative, quantile)</td></tr>
<tr><td valign=\"top\" width=\"150\">Normal</td>
    <td valign=\"top\"> Library of normal distribution functions (functions: density, cumulative, quantile)</td></tr>
<tr><td valign=\"top\" width=\"150\">TruncatedNormal </td>
    <td valign=\"top\"> Library of truncated normal distribution functions (functions: density, cumulative, quantile)</td></tr>
<tr><td valign=\"top\" width=\"150\">Weibull</td>
    <td valign=\"top\"> Library of Weibull distribution functions (functions: density, cumulative, quantile)</td></tr>
<tr><td valign=\"top\" width=\"150\">TruncatedWeibull </td>
    <td valign=\"top\"> Library of truncated Weibull distribution functions (functions: density, cumulative, quantile)</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Special.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">erf</td>
    <td valign=\"top\">Error function erf(u) = 2/sqrt(pi)*Integral_0_u exp(-t^2)*d</td></tr>
<tr><td valign=\"top\" width=\"150\">erfc</td>
    <td valign=\"top\">Complementary error function erfc(u) = 1 - erf(u)</td></tr>
<tr><td valign=\"top\" width=\"150\">erfInv</td>
    <td valign=\"top\">Inverse error function: u = erf(erfInv(u))</td></tr>
<tr><td valign=\"top\" width=\"150\">erfcInv </td>
    <td valign=\"top\">Inverse complementary error function: u = erfc(erfcInv(u))</td></tr>
<tr><td valign=\"top\" width=\"150\">sinc </td>
    <td valign=\"top\">Unnormalized sinc function: sinc(u) = sin(u)/u</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.FastFourierTransform.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">realFFTinfo </td>
    <td valign=\"top\">Print information about real FFT for given f_max and f_resolution</td></tr>
<tr><td valign=\"top\" width=\"150\">realFFTsamplePoints </td>
    <td valign=\"top\">Return number of sample points for a real FFT</td></tr>
<tr><td valign=\"top\" width=\"150\">realFFT</td>
    <td valign=\"top\">Return amplitude and phase vectors for a real FFT</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Utilities.Streams.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">readMatrixSize</td>
    <td valign=\"top\">Read dimensions of a Real matrix from a MATLAB MAT file</td></tr>
<tr><td valign=\"top\" width=\"150\">readRealMatrix</td>
    <td valign=\"top\">Read Real matrix from MATLAB MAT file</td></tr>
<tr><td valign=\"top\" width=\"150\">writeRealMatrix</td>
    <td valign=\"top\">Write Real matrix to a MATLAB MAT file</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Utilities.Strings.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">hashString</td>
    <td valign=\"top\">Creates a hash value of a String</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Utilities.System.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">getTime</td>
    <td valign=\"top\">Retrieves the local time (in the local time zone)</td></tr>
<tr><td valign=\"top\" width=\"150\">getPid</td>
    <td valign=\"top\">Retrieves the current process id</td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">existing components</b> have been <b style=\"color:blue\">changed</b> in a <b style=\"color:blue\">non-backward compatible</b> way:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Electrical.Analog.Semiconductors.</b></td></tr>
<tr><td valign=\"top\"> HeatingDiode </td>
          <td valign=\"top\"> Removed protected variable k \"Boltzmann's constant\".<br>
                            Calculate protected constant q \"Electron charge\" from already known constants instead of defining a protected variable q.</td></tr>
<tr><td valign=\"top\"> HeatingNPN<br>
                      HeatingPNP </td>
          <td valign=\"top\"> Removed parameter K \"Boltzmann's constant\" and q \"Elementary electronic charge\".<br>
                            Calculate instead protected constant q \"Electron charge\" from already known constants.<br>
                            Users that have used these parameters might have broken their models;
                            the (although formal non-backwards compatible) change offers the users a safer use.</td></tr>
</table>

</html>"));
end Version_3_2_2;

  extends Modelica.Icons.ReleaseNotes;

class Version_3_2_1 "Version 3.2.1 (August 14, 2013)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>
<p>
Version 3.2.1 is backward compatible to version 3.2, that is models developed with
versions 3.0, 3.0.1, 3.1, or 3.2 will work without any changes also with version 3.2.1.
This version is a \"clean-up\" with major emphasis on quality improvement and
tool compatibility. The goal is that all
<a href=\"https://www.modelica.org/tools\">Modelica tools</a> will support this package
and will interpret it in the same way. Short Overview:
</p>

<ul>
<li> This version of the Modelica package is <b>fully compatible</b> to
     Modelica Specification <b>3.2 revision 2</b>.<br>
     (Especially, some operators used in package Modelica,
     such as \"rooted\", have been standardized in 3.2 rev. 2,
     as well as vendor specific annotations. Furthermore,
     ambiguous/unclear descriptions in the specification have
     been corrected/improved. One important improvement in packages
     Modelica and ModelicaTest is that the initialization has been fully defined
     in all example models, in order that all tools can produce the same result
     without relying on tool heuristics).
     </li>

<li> About <a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.1/ResolvedTracTickets.html\">400 tickets</a>
     have been fixed for this release, and
     especially all compliance issues and all relevant defect issues.
     </li>

<li> An open source implementation of the <b>table blocks</b> has been provided
     by <a href=\"http://www.itisim.com\">ITI GmbH</a>. This work has been
     <a href=\"https://www.modelica.org/news_items/call-texts-to-improve-modelica-2012/2012-12-20-Call-for-quotation-for-MSL-tables.pdf/at_download/file\">paid by Modelica Association</a>.
     As a result, all parts of package Modelica are now available
     in a free implementation. Additionally new features have been added to the table blocks
     by this implementation:
     <ul>
     <li>The table outputs can be differentiated once.</li>
     <li>Support of binary MATLAB MAT-file formats v6 and v7</li>
     <li>New option ConstantSegments for parameter Smoothness</li>
     <li>New option NoExtrapolation for parameter Extrapolation</li>
     <li>Support of tables provided in the C-Code (usertab.c, for realtime systems without file system)</li>
     </ul></li>

<li> <b>Icons</b> have been re-designed by Wolfram Research to provide a more modern view.</li>

<li> The <b>Modelica.Media.Air.MoistAir</b> media model has been improved so that it
     can be used in a temperature range of 190 ... 647 K (previously: 240 ... 400 K).</li>

<li> New media models for air (<b>ReferenceAir</b> with a large operating range: 30 ... 2000 K,
     0 ... 2000 MPa), for moist air (<b>ReferenceMoistAir</b> with a large operating range:
     143.15 ... 2000 K, 0 .. 10 MPa;  but 1-2 orders of magnitude slower as
     Modelica.Media.Air.MoistAir),
     and the refrigerant <b>R134a</b> are included in the Modelica.Media library in order to
     improve the modeling of air conditioning systems especially in aircraft.
     These models have been developed by
     <a href=\"http://www.xrg-simulation.de/\">XRG Simulation GmbH</a>
     as part of the <a href=\"http://www.cleansky.eu/\">Clean Sky</a> JTI project
     (Project number: 296369; Theme: JTI-CS-2011-1-SGO-02-026).
     The partial financial support by the European Union for this development
     is highly appreciated.</li>

<li> <b>60</b> models and blocks and <b>90</b> functions are newly included, for details see below.</li>

</ul>

<p>
This release of package Modelica, and the accompanying ModelicaTest, has been tested with the
following tools (the tools are listed alphabetically. At the time of the test, some of the
tools might not yet supported the complete Modelica package):
</p>

<ul>
<li> CyModelica </li>
<li> Dymola 2014 (Windows 64 bit)<br>
     Regression test results with regards to Modelica 3.2 are available
     in ticket <a href=\"https://trac.modelica.org/Modelica/ticket/1114\">#1114</a>.</li>
<li> Dymola 2014 FD01 development with pedantic flag (Windows 64 bit)<br>
     (\"pedantic flag\" means that strict Modelica compliance is checked.
     Dymola 2014 fails with pedantic flag, e.g., because the annotation DocumentationClass
     was not standardized when this version of Dymola was released).</li>
<li> Maplesim Parser</li>
<li> MWorks 3.2</li>
<li> OpenModelica 1.9.0 Beta4+dev (Windows, Linux, Mac)<br>
     Test reports for the daily builds are available
     <a href=\"https://trac.openmodelica.org/OpenModelica/wiki\">here</a>.
     Test reports of comparisons with Dymola result files are available
     <a href=\"https://test.openmodelica.org/hudson/job/OpenModelica_TEST_CLANG/lastCompletedBuild/testReport/(root)/simulation_libraries_msl32/\">here</a>.
     </li>
<li> SimulationX 3.6</li>
</ul>

<p>
The following Modelica packages have been tested that they work together with this release of package Modelica
(alphabetical list):
</p>

<ul>
<li> Buildings 1.4 (LBNL) </li>
<li> FlexibleBodies 2.0.1 (DLR)</li>
<li> Modelica_Synchronous 0.91 (DLR)</li>
<li> Optimization 2.2 (DLR)</li>
<li> PowerTrain 2.2.0 (DLR) </li>
</ul>

<p>
The new open source tables have been tested by T. Beutlich (ITI):
</p>

<ul>
<li> 193 Modelica test models for compatibility check with previous table implementation
     (available in ModelicaTest.Tables).
     Performed tests with SimulationX 3.5.707 (32 bit) and
     Dymola 2013 FD01 (32 bit). Furthermore a basic check was performed in OpenModelica
     to make sure it works in general.
     </li>
<li> The two C source files (Modelica/Resources/C-Sources/ModelicaStandardTables.c; ModelicaMatIO.c)
     have been tested to successfully compile for the following platforms<br>
     &nbsp;&nbsp;&nbsp;Windows 32 and 64 bit<br>
     &nbsp;&nbsp;&nbsp;Linux 32 and 64 bit<br>
     &nbsp;&nbsp;&nbsp;dSPACE SCALEXIO<br>
     &nbsp;&nbsp;&nbsp;dSPACE DS1005 (no file system)<br>
     &nbsp;&nbsp;&nbsp;dSPACE DS1006 (no file system)<br>
     &nbsp;&nbsp;&nbsp;dSPACE DS1401 (no file system)
     </li>
<li> The following compilers/environments have been used for the platform evaluation<br>
     &nbsp;&nbsp;&nbsp;Microsoft compilers (VC6 and &ge; VS2005 (Win32 and x64)) <br>
     &nbsp;&nbsp;&nbsp;MinGW (GCC 4.4.0 and GCC 4.7.2)<br>
     &nbsp;&nbsp;&nbsp;Cygwin (GCC 4.3.0)<br>
     &nbsp;&nbsp;&nbsp;Open WATCOM 1.3<br>
     &nbsp;&nbsp;&nbsp;LCC 2.4.1<br>
     &nbsp;&nbsp;&nbsp;Borland C/C++ (free command line tools) 5.5<br>
     &nbsp;&nbsp;&nbsp;GCC 4.x on Linux<br>
     &nbsp;&nbsp;&nbsp;GCC 3.3.5 (for DS1006)<br>
     &nbsp;&nbsp;&nbsp;Microtec PowerPC Compiler 3.7 (for DS1005)
     </li>
</ul>

<p>
The exact difference between package Modelica version 3.2 and version 3.2.1 is
summarized in a
<a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.1/DifferencesTo32.html\">comparison table</a>.
</p>

<p>
About <b>400</b> trac tickets have been fixed for this release. An overview is given
<a href=\"modelica://Modelica/Resources/Documentation/Version-3.2.1/ResolvedTracTickets.html\">here</a>.
Clicking on a ticket gives all information about it.
</p>


<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.Blocks.Logical.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> RSFlipFlop</td>
    <td valign=\"top\"> Basic RS flip flop</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Math.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> MinMax</td>
    <td valign=\"top\">Output the minimum and the maximum element of the input vector </td></tr>
<tr><td valign=\"top\" width=\"150\"> LinearDependency </td>
    <td valign=\"top\">Output a linear combination of the two inputs </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Nonlinear.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> SlewRateLimiter</td>
    <td valign=\"top\"> Limit the slew rate of a signal </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Memories</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> DLATRAM</td>
    <td valign=\"top\"> Level sensitive Random Access Memory </td></tr>
<tr><td valign=\"top\" width=\"150\"> DLATROM</td>
    <td valign=\"top\"> Level sensitive Read Only Memory </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Multiplexers</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> MUX2x1</td>
    <td valign=\"top\"> A two inputs MULTIPLEXER for multiple value logic (2 data inputs, 1 select input, 1 output) </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Examples.AsynchronousInductionMachines.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> AIMC_Initialize </td>
    <td valign=\"top\"> Steady-State Initialization example of AsynchronousInductionMachineSquirrelCage </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Examples.SynchronousInductionMachines.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> SMPM_VoltageSource </td>
    <td valign=\"top\"> PermanentMagnetSynchronousInductionMachine example fed by FOC </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Examples.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> TestSensors </td>
    <td valign=\"top\"> Example for multiphase quasiRMS sensors: A sinusoidal source feeds a load consisting of resistor and inductor </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Sensors.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> VoltageQuasiRMSSensor </td>
    <td valign=\"top\"> Continuous quasi voltage RMS sensor for multi phase system </td></tr>
<tr><td valign=\"top\" width=\"150\"> CurrentQuasiRMSSensor </td>
    <td valign=\"top\"> Continuous quasi current RMS sensor for multi phase system </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Blocks.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> QuasiRMS </td>
    <td valign=\"top\"> Determine quasi RMS value of a multi-phase system </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Functions.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> quasiRMS </td>
    <td valign=\"top\"> Calculate continuous quasi RMS value of input </td></tr>
<tr><td valign=\"top\" width=\"150\"> activePower </td>
    <td valign=\"top\"> Calculate active power of voltage and current input </td></tr>
<tr><td valign=\"top\" width=\"150\"> symmetricOrientation </td>
    <td valign=\"top\"> Orientations of the resulting fundamental wave field phasors </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Spice3.Examples.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> CoupledInductors<br>
                                      CascodeCircuit<br>
                                      Spice3BenchmarkDifferentialPair<br>
                                      Spice3BenchmarkMosfetCharacterization<br>
                                      Spice3BenchmarkRtlInverter<br>
                                      Spice3BenchmarkFourBitBinaryAdder</td>
    <td valign=\"top\"> Spice3 examples and benchmarks from the SPICE3 Version e3 User's Manual </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Spice3.Basic.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> K_CoupledInductors</td>
    <td valign=\"top\"> Inductive coupling via coupling factor K </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Spice3.Semiconductors.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> M_NMOS2 <br>
                                      M_PMOS2 <br>
                                      ModelcardMOS2</td>
    <td valign=\"top\">  N/P channel MOSFET transistor with fixed level 2 </td></tr>
<tr><td valign=\"top\" width=\"150\"> J_NJFJFE <br>
                                      J_PJFJFE <br>
                                      ModelcardJFET</td>
    <td valign=\"top\">  N/P-channel junction field-effect transistor </td></tr>
<tr><td valign=\"top\" width=\"150\"> C_Capacitor <br>
                                      ModelcardCAPACITOR</td>
    <td valign=\"top\">  Semiconductor capacitor model </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> AIMC_DOL_MultiPhase<br>
                                      AIMS_Start_MultiPhase<br>
                                      SMPM_Inverter_MultiPhase<br>
                                      SMEE_Generator_MultiPhase<br>
                                      SMR_Inverter_MultiPhase</td>
    <td valign=\"top\"> Multi-phase machine examples </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Sensors.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> MassFractions<br>
                                      MassFractionsTwoPort</td>
    <td valign=\"top\"> Ideal mass fraction sensors </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Media.</b></td></tr>
<tr><td valign=\"top\" width=\"150\">R134a</td>
    <td valign=\"top\"> R134a (Tetrafluoroethane) medium model in the range (0.0039 bar .. 700 bar,
    169.85 K .. 455 K)</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Media.Air.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> ReferenceAir</td>
    <td valign=\"top\"> Detailed dry air model with a large operating range (130 ... 2000 K, 0 ... 2000 MPa)
                        based on Helmholtz equations of state</td></tr>
<tr><td valign=\"top\" width=\"150\"> ReferenceMoistAir</td>
    <td valign=\"top\"> Detailed moist air model (143.15 ... 2000 K)</td></tr>
<tr><td valign=\"top\" width=\"150\"> MoistAir</td>
    <td valign=\"top\"> Temperature range of functions of MoistAir medium enlarged from
                        240 - 400 K to  190 - 647 K.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Media.Air.MoistAir.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> velocityOfSound<br>
                                      isobaricExpansionCoefficient<br>
                                      isothermalCompressibility<br>
                                      density_derp_h<br>
                                      density_derh_p<br>
                                      density_derp_T<br>
                                      density_derT_p<br>
                                      density_derX<br>
                                      molarMass<br>
                                      T_psX<br>
                                      setState_psX<br>
                                      s_pTX<br>
                                      s_pTX_der<br>
                                      isentropicEnthalpy</td>
    <td valign=\"top\"> Functions returning additional properties of the moist air medium model</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Thermal.HeatTransfer.Components.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> ThermalResistor</td>
    <td valign=\"top\"> Lumped thermal element transporting heat without storing it (dT = R*Q_flow) </td></tr>
<tr><td valign=\"top\" width=\"150\"> ConvectiveResistor</td>
    <td valign=\"top\"> Lumped thermal element for heat convection (dT = Rc*Q_flow) </td></tr>

<tr><td colspan=\"2\"><b>Modelica.MultiBody.Examples.Constraints.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> PrismaticConstraint <br>
                        RevoluteConstraint<br>
                        SphericalConstraint<br>
                        UniversalConstraint</td>
    <td valign=\"top\"> Demonstrates the use of the new Joints.Constraints joints by comparing
                        them with the standard joints.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.MultiBody.Joints.Constraints.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> Prismatic <br>
                        Revolute<br>
                        Spherical<br>
                        Universal</td>
    <td valign=\"top\"> Joint elements formulated as kinematic constraints. These elements are
                        designed to break kinematic loops and result usually in numerically more
                        efficient and reliable loop handling as the (standard) automatic handling.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> MultiSensor</td>
    <td valign=\"top\"> Ideal sensor to measure the torque and power between two flanges and the absolute angular velocity </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Translational.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> MultiSensor</td>
    <td valign=\"top\"> Ideal sensor to measure the absolute velocity, force and power between two flanges </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> isPowerOf2</td>
    <td valign=\"top\"> Determine if the integer input is a power of 2 </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Math.Vectors.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> normalizedWithAssert</td>
    <td valign=\"top\"> Return normalized vector such that length = 1 (trigger an assert for zero vector) </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Math.BooleanVectors.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> countTrue</td>
    <td valign=\"top\"> Returns the number of true entries in a Boolean vector  </td></tr>
<tr><td valign=\"top\" width=\"150\"> enumerate</td>
    <td valign=\"top\"> Enumerates the true entries in a Boolean vector (0 for false entries) </td></tr>
<tr><td valign=\"top\" width=\"150\"> index</td>
    <td valign=\"top\"> Returns the indices of the true entries of a Boolean vector</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Utilities.Files.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> loadResource</td>
    <td valign=\"top\"> Return the absolute path name of a URI or local file name  </td></tr>

<tr><td colspan=\"2\"><b>Modelica.SIunits.</b></td></tr>
<tr><td valign=\"top\" width=\"150\"> PressureDifference<br>
                        MolarDensity<br>
                        MolarEnergy<br>
                        MolarEnthalpy<br>
                        TimeAging<br>
                        ChargeAging<br>
                        PerUnit<br>
                        DerPressureByDensity<br>
                        DerPressureByTemperature</td>
    <td valign=\"top\"> New SI unit types </td></tr>
</table>
</html>"));
end Version_3_2_1;

class Version_3_2 "Version 3.2 (Oct. 25, 2010)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p>
Version 3.2 is backward compatible to version 3.1, i.e., models developed with
versions 3.0, 3.0.1, or 3.1 will work without any changes also with version 3.2.
This version is a major improvement:
</p>

<ul>
<li> <b>357</b> models and blocks and <b>295</b> functions are newly included.</li>

<li><b>7</b> new libraries are included.</li>

<li> The icons of the library are newly designed to provide a modern, unified view,
     see <a href=\"modelica://Modelica.Icons\">Modelica.Icons</a>.</li>

<li> All non-Modelica files, such as images, pdf-files, C-source files,
     scripts are moved to the new directory \"Modelica\\Resources\".
     Furthermore, all file references are changed to URIs as introduced in
     Modelica 3.1 (e.g., a file with the file name
     \"Modelica/Resources/Images/xxx\" is referenced as
     \"modelica://Modelica/Resources/Images/xxx\").</li>

<li> All physical models that dissipate heat (such as electrical elements,
     electrical machines, bearings, dampers, etc.), have now an optional heat port
     to which the dissipated energy is flowing, if activated.
     This will significantly improve design studies about the thermal efficiency
     of technical systems.</li>

<li> All electrical machines in the
     <a href=\"modelica://Modelica.Electrical.Machines\">Machines</a>
     library have now a \"Losses\" tab in the parameter menu to optionally
     model machines losses such as frictional losses, stator core losses
     or stray load losses, respectively.</li>

<li> All electrical machines in the
     <a href=\"modelica://Modelica.Electrical.Machines\">Machines</a>
     library have now a \"powerBalance\" result record,
     summarizing converted power and losses.</li>
</ul>

<p>
Version 3.2 is slightly based on the Modelica Specification 3.2. It uses
the following new language elements (compared to Modelica Specification 3.1):
</p>

<ul>
<li> Operator records and overloaded operators.</li>
<li> Functions as input arguments to functions.</li>
<li> Improved expandable connectors (variables declared in expandable
     connectors are ignored if not referenced).</li>
</ul>

<p>
A large part of the new classes have been developed with
partial financial support by
<a href=\"http://www.bmbf.de/en/index.php\">BMBF</a>
(BMBF F&ouml;rderkennzeichen: 01IS07022F)
within the <a href=\"http://www.itea2.org\">ITEA2</a> project
EUROSYSLIB.
We highly appreciate this funding.
</p>

<p>
The following <b style=\"color:blue\">new libraries</b> have been added:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Complex\">Complex</a></td>
    <td valign=\"top\">
    This is a top-level record outside of the Modelica Standard Library.
    It is used for complex numbers and contains overloaded operators.
    From a users point of view, Complex is used in a similar way as the
    built-in type Real. Example:<br>
    <code>&nbsp;  Real     a    = 2;</code><br>
    <code>&nbsp;  Complex  j    = Modelica.ComplexMath.j;</code><br>
    <code>&nbsp;  Complex  b    = 2 + 3*j;</code><br>
    <code>&nbsp;  Complex  c    = (2*b + a)/b;</code><br>
    <code>&nbsp;  Complex  d    = Modelica.ComplexMath.sin(c);</code><br>
    <code>&nbsp;  Complex  v[3] = {b/2, c, 2*d};</code><br>
    (This library was developed by Marcus Baur, DLR).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.ComplexBlocks\">Modelica.ComplexBlocks</a></td>
    <td valign=\"top\">
    Library of basic input/output control blocks with Complex signals.<br>
    This library is especially useful in combination with the new
    <a href=\"modelica://Modelica.Electrical.QuasiStationary\">Modelica.Electrical.QuasiStationary</a>
    library in order to build up very fast simulations of electrical circuits with periodic
    currents and voltages.<br>
    (This library was developed by Anton Haumer).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Electrical.QuasiStationary\">Modelica.Electrical.QuasiStationary</a></td>
    <td valign=\"top\">
    Library for quasi-stationary electrical singlephase and multiphase AC simulation.<br>
    This library allows very fast simulations of electrical circuits with sinusoidal
    currents and voltages by only taking into account the quasi-stationary, periodic part
    and neglecting non-periodic transients.<br>
    (This library was developed by Anton Haumer and Christian Kral).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Spice3\">Modelica.Electrical.Spice3</a></td>
    <td valign=\"top\">
    Library with components of the Berkeley
    <a href=\"http://bwrc.eecs.berkeley.edu/Classes/IcBook/SPICE/\">SPICE3</a>
    simulator:<br>
    R, C, L, controlled and independent sources, semiconductor device models
    (MOSFET Level 1, Bipolar junction transistor, Diode, Semiconductor resistor).
    The components have been intensively tested with more than 1000 test models
    and compared with results from the SPICE3 simulator. All test models give identical
    results in Dymola 7.4 with respect to the Berkeley SPICE3 simulator up to the relative
    tolerance of the integrators.<br>
    This library allows detailed simulations of electronic circuits.
    Work on Level 2 SPICE3 models, i.e., even more detailed models, is under way.
    Furthermore, a pre-processor is under development to transform automatically
    a SPICE netlist into a Modelica model, in order that the many available
    SPICE3 models can be directly used in a Modelica model.<br>
    (This library was developed by Fraunhofer Gesellschaft, Dresden).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Magnetic.FundamentalWave\">Modelica.Magnetic.FundamentalWave</a></td>
    <td valign=\"top\">
     Library for magnetic fundamental wave effects in electric machines for the
     application in three phase electric machines.
     The library is an alternative approach to the Modelica.Electrical.Machines library.
     A great advantage of this library is the strict object orientation of the
     electrical and magnetic components that the electric machines models are composed of.
     This allows an easier incorporation of more detailed physical effects of
     electrical machines.
     From a didactic point of view this library is very beneficial for students in the field
     of electrical engineering.<br>
     (This library was developed by Christian Kral and Anton Haumer, using
     ideas and source code of a library from Michael Beuschel from 2000).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Fluid.Dissipation\">Modelica.Fluid.Dissipation</a></td>
    <td valign=\"top\">
     Library with functions to compute convective heat transfer and pressure loss characteristics.<br>
     (This library was developed by Thorben Vahlenkamp and Stefan Wischhusen from
     XRG Simulation GmbH).
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.ComplexMath\">Modelica.ComplexMath</a></td>
    <td valign=\"top\">
    Library of complex mathematical functions (e.g., sin, cos) and of functions operating
    on complex vectors.<br>
    (This library was developed by Marcus Baur from DLR-RM, Anton Haumer, and
     HansJ&uuml;rg Wiesmann).
    </td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.UsersGuide</b></td></tr>
<tr><td valign=\"top\"> Conventions
                      </td>
    <td valign=\"top\"> Considerably improved 'Conventions' for the Modelica Standard Library.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Examples</b></td></tr>
<tr><td valign=\"top\"> Filter<br>
                      FilterWithDifferentation<br>
                      FilterWithRiseTime<br>
                      RealNetwork1<br>
                      IntegerNetwork1<br>
                      BooleanNetwork1<br>
                      Interaction1
                      </td>
    <td valign=\"top\"> Examples for the newly introduced block components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Continuous</b></td></tr>
<tr><td valign=\"top\"> Filter </td>
    <td valign=\"top\"> Continuous low pass, high pass, band pass and band stop
                      IIR-filter of type CriticalDamping, Bessel, Butterworth and Chebyshev I.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Interaction.Show</b></td></tr>
<tr><td valign=\"top\"> RealValue<br>
                      IntegerValue<br>
                      BooleanValue</td>
    <td valign=\"top\"> Blocks to show the values of variables in a diagram animation.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Interfaces</b></td></tr>
<tr><td valign=\"top\"> RealVectorInput<br>
                      IntegerVectorInput<br>
                      BooleanVectorInput<br>
                      PartialRealMISO<br>
                      PartialIntegerSISO<br>
                      PartialIntegerMISO<br>
                      PartialBooleanSISO_small<br>
                      PartialBooleanMISO
                      </td>
    <td valign=\"top\"> Interfaces and partial blocks for the new block components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Math</b></td></tr>
<tr><td valign=\"top\"> MultiSum<br>
                      MultiProduct<br>
                      MultiSwitch </td>
    <td valign=\"top\"> Sum, product and switch blocks with 1,2,...,N inputs
                      (based on connectorSizing annotation to handle vectors of
                       connectors in a convenient way).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.MathInteger</b></td></tr>
<tr><td valign=\"top\"> MultiSwitch<br>
                      Sum<br>
                      Product<br>
                      TriggeredAdd</td>
    <td valign=\"top\"> Mathematical blocks for Integer signals.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Boolean</b></td></tr>
<tr><td valign=\"top\"> MultiSwitch<br>
                      And<br>
                      Or<br>
                      Xor<br>
                      Nand<br>
                      Nor<br>
                      Not<br>
                      RisingEdge<br>
                      FallingEdge<br>
                      ChangingEdge<br>
                      OnDelay</td>
    <td valign=\"top\"> Mathematical blocks for Boolean signals.
                      Some of these blocks are available also in library
                      <a href=\"modelica://Modelica.Blocks.Logical\">Logical</a>.
                      The new design is based on the connectorSizing annotation that allows
                      the convenient handling of an arbitrary number of input signals
                      (e.g., the \"And\" block has 1,2,...,N inputs, instead of only 2 inputs
                      in the <a href=\"modelica://Modelica.Blocks.Logical\">Logical</a> library).
                      Additionally, the icons are smaller so that the diagram area is
                      better utilized</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Sources</b></td></tr>
<tr><td valign=\"top\"> RadioButtonSource</td>
    <td valign=\"top\"> Boolean signal source that mimics a radio button.</td></tr>
<tr><td valign=\"top\"> IntegerTable</td>
    <td valign=\"top\"> Generate an Integer output signal based on a table matrix
                      with [time, yi] values.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Examples</b></td></tr>
<tr><td valign=\"top\"> SimpleTriacCircuit,<br>
                      IdealTriacCircuit,<br>
                      AD_DA_conversion </td>
    <td valign=\"top\"> Examples for the newly introduced Analog components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Ideal</b></td></tr>
<tr><td valign=\"top\"> IdealTriac,<br>
                      AD_Converter,<br>
                      DA_Converter </td>
    <td valign=\"top\"> AD and DA converter, ideal triac (based on ideal thyristor).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Semiconductors</b></td></tr>
<tr><td valign=\"top\"> SimpleTriac </td>
    <td valign=\"top\"> Simple triac based on semiconductor thyristor model.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Examples</b></td></tr>
<tr><td valign=\"top\">  Delay_example,<br>
                       DFFREG_example,<br>
                       DFFREGL_example,<br>
                       DFFREGSRH_example,<br>
                       DFFREGSRL_example,<br>
                       DLATREG_example,<br>
                       DLATREGL_example,<br>
                       DLATREGSRH_example,<br>
                       DLATREGSRL_example,<br>
                       NXFER_example,<br>
                       NRXFER_example,<br>
                       BUF3S_example,<br>
                       INV3S_example,<br>
                       WiredX_example </td>
    <td valign=\"top\"> Examples for the newly introduced Digital components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Interfaces</b></td></tr>
<tr><td valign=\"top\"> UX01,<br>
                      Strength,<br>
                      MIMO </td>
    <td valign=\"top\"> Interfaces for the newly introduced Digital components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Tables</b></td></tr>
<tr><td valign=\"top\"> ResolutionTable,<br>
                      StrengthMap,<br>
                      NXferTable,<br>
                      NRXferTable,<br>
                      PXferTable,<br>
                      PRXferTable,<br>
                      Buf3sTable,<br>
                      Buf3slTable </td>
    <td valign=\"top\"> New Digital table components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Delay</b></td></tr>
<tr><td valign=\"top\"> InertialDelaySensitiveVector </td>
    <td valign=\"top\"> New Digital delay component.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Registers</b></td></tr>
<tr><td valign=\"top\"> DFFR,<br>
                      DFFREG,<br>
                      DFFREGL,<br>
                      DFFSR,<br>
                      DFFREGSRH,<br>
                      DFFREGSRL,<br>
                      DLATR,<br>
                      DLATREG,<br>
                      DLATREGL,<br>
                      DLATSR,<br>
                      DLATREGSRH,<br>
                      DLATREGSRL </td>
    <td valign=\"top\"> Various register components (collection of flipflops and latches)
                      according to the VHDL standard.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Tristates</b></td></tr>
<tr><td valign=\"top\"> NXFERGATE,<br>
                      NRXFERGATE,<br>
                      PXFERGATE,<br>
                      PRXFERGATE,<br>
                      BUF3S,<br>
                      BUF3SL,<br>
                      INV3S,<br>
                      INV3SL,<br>
                      WiredX </td>
    <td valign=\"top\"> Transfer gates, buffers, inverters and wired node.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Basic</b></td></tr>
<tr><td valign=\"top\"> MutualInductor </td>
    <td valign=\"top\"> Multi phase inductor providing a mutual inductance matrix model.</td></tr>
<tr><td valign=\"top\"> ZeroInductor </td>
    <td valign=\"top\"> Multi phase zero sequence inductor.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines</b></td></tr>
<tr><td valign=\"top\"> Examples </td>
    <td valign=\"top\"> Structured according to machine types:<br>
                      AsynchronousInductionMachines<br>
                      SynchronousInductionMachines<br>
                      DCMachines<br>
                      Transformers </td></tr>
<tr><td valign=\"top\"> Losses.* </td>
    <td valign=\"top\"> Parameter records and models for losses in electrical machines and transformers (where applicable): <br>
                      Friction losses <br>
                      Brush losses <br>
                      Stray Load losses <br>
                      Core losses (only eddy current losses but no hysteresis losses; not for transformers) </td></tr>
<tr><td valign=\"top\"> Thermal.* </td>
    <td valign=\"top\"> Simple thermal ambients, to be connected to the thermal ports of machines, <br>
                      as well as material constants and utility functions.</td></tr>
<tr><td valign=\"top\"> Icons.* </td>
    <td valign=\"top\"> Icons for transient and quasistationary electrical machines and transformers.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Examples.AsynchronousInductionMachines.</b></td></tr>
<tr><td valign=\"top\"> AIMC_withLosses </td>
    <td valign=\"top\"> Asynchronous induction machine with squirrel cage with losses </td></tr>
<tr><td valign=\"top\"> AIMC_Transformer </td>
    <td valign=\"top\"> Asynchronous induction machine with squirrel cage - transformer starting </td></tr>
<tr><td valign=\"top\"> AIMC_withLosses </td>
    <td valign=\"top\"> Test example of an asynchronous induction machine with squirrel cage with losses </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Examples.SynchronousInductionMachines.</b></td></tr>
<tr><td valign=\"top\"> SMPM_CurrentSource </td>
    <td valign=\"top\"> Permanent magnet synchronous induction machine fed by a current source </td></tr>
<tr><td valign=\"top\"> SMEE_LoadDump </td>
    <td valign=\"top\"> Electrical excited synchronous induction machine with voltage controller </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Examples.DCMachines.</b></td></tr>
<tr><td valign=\"top\"> DCSE_SinglePhase </td>
    <td valign=\"top\"> Series excited DC machine, fed by sinusoidal voltage </td></tr>
<tr><td valign=\"top\"> DCPM_Temperature </td>
    <td valign=\"top\"> Permanent magnet DC machine, demonstration of varying temperature </td></tr>
<tr><td valign=\"top\"> DCPM_Cooling </td>
    <td valign=\"top\"> Permanent magnet DC machine, coupled with a simple thermal model </td></tr>
<tr><td valign=\"top\"> DCPM_QuasiStationary </td>
    <td valign=\"top\"> Permanent magnet DC machine, comparison between transient and quasistationary model </td></tr>
<tr><td valign=\"top\"> DCPM_Losses </td>
    <td valign=\"top\"> Permanent magnet DC machine, comparison between model with and without losses </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.BasicMachines.QuasiStationaryDCMachines.</b></td></tr>
<tr><td valign=\"top\"> DC_PermanentMagnet <br>
                      DC_ElectricalExcited <br>
                      DC_SeriesExcited </td>
    <td valign=\"top\"> QuasiStationary DC machines, i.e., neglecting electrical transients </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.BasicMachines.Components.</b></td></tr>
<tr><td valign=\"top\"> InductorDC </td>
    <td valign=\"top\"> Inductor model which neglects der(i) if Boolean parameter quasiStationary = true </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Interfaces.</b></td></tr>
<tr><td valign=\"top\">  ThermalPortTransformer <br>
                       PowerBalanceTransformer </td>
    <td valign=\"top\"> Thermal ports and power balances for electrical machines and transformers.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Utilities</b></td></tr>
<tr><td valign=\"top\"> SwitchedRheostat </td>
    <td valign=\"top\"> Switched rheostat, used for starting asynchronous induction motors with slipring rotor.</td></tr>
<tr><td valign=\"top\"> RampedRheostat </td>
    <td valign=\"top\"> Ramped rheostat, used for starting asynchronous induction motors with slipring rotor.</td></tr>
<tr><td valign=\"top\"> SynchronousMachineData </td>
    <td valign=\"top\"> The parameters of the synchronous machine model with electrical excitation (and damper) are calculated
                      from parameters normally given in a technical description,
                      according to the standard EN 60034-4:2008 Appendix C.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Examples.Elementary.</b></td></tr>
<tr><td valign=\"top\"> HeatLosses </td>
    <td valign=\"top\"> Demonstrate the modeling of heat losses.</td></tr>
<tr><td valign=\"top\"> UserDefinedGravityField </td>
    <td valign=\"top\"> Demonstrate the modeling of a user-defined gravity field.</td></tr>
<tr><td valign=\"top\"> Surfaces </td>
    <td valign=\"top\"> Demonstrate the visualization of a sine surface,<br>
                      as well as a torus and a wheel constructed from a surface.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Joints.</b></td></tr>
<tr><td valign=\"top\"> FreeMotionScalarInit </td>
    <td valign=\"top\"> Free motion joint that allows initialization and state selection<br>
                      of single elements of the relevant vectors<br>
                      (e.g., initialize r_rel_a[2] but not the other elements of r_rel_a;<br>
                      this new component fixes ticket
                      <a href=\"https://trac.modelica.org/Modelica/ticket/274\">#274</a>) </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.</b></td></tr>
<tr><td valign=\"top\"> Torus </td>
    <td valign=\"top\"> Visualizing a torus.</td></tr>
<tr><td valign=\"top\"> VoluminousWheel </td>
    <td valign=\"top\"> Visualizing a voluminous wheel.</td></tr>
<tr><td valign=\"top\"> PipeWithScalarField </td>
    <td valign=\"top\"> Visualizing a pipe with scalar field quantities along the pipe axis.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.ColorMaps.</b></td></tr>
<tr><td valign=\"top\"> jet<br>
                      hot<br>
                      gray<br>
                      spring<br>
                      summer<br>
                      autumn<br>
                      winter </td>
    <td valign=\"top\"> Functions returning different color maps.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.Colors.</b></td></tr>
<tr><td valign=\"top\"> colorMapToSvg </td>
    <td valign=\"top\"> Save a color map on file in svg (scalable vector graphics) format.</td></tr>
<tr><td valign=\"top\"> scalarToColor </td>
    <td valign=\"top\"> Map a scalar to a color using a color map.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.Advanced.</b></td></tr>
<tr><td valign=\"top\"> Surface </td>
    <td valign=\"top\"> Visualizing a moveable, parameterized surface;<br>
                      the surface characteristic is provided by a function<br>
                      (this new component fixes ticket
                       <a href=\"https://trac.modelica.org/Modelica/ticket/181\">#181</a>)</td></tr>
<tr><td valign=\"top\"> PipeWithScalarField </td>
    <td valign=\"top\"> Visualizing a pipe with a scalar field.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.Advanced.SurfaceCharacteristics.</b></td></tr>
<tr><td valign=\"top\"> torus </td>
    <td valign=\"top\"> Function defining the surface characteristic of a torus.</td></tr>
<tr><td valign=\"top\"> pipeWithScalarField </td>
    <td valign=\"top\"> Function defining the surface characteristic of a pipe<br>
                      where a scalar field value is displayed with color along the pipe axis.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.Examples.</b></td></tr>
<tr><td valign=\"top\"> HeatLosses </td>
    <td valign=\"top\"> Demonstrate the modeling of heat losses.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Translational.Examples.</b></td></tr>
<tr><td valign=\"top\"> HeatLosses </td>
    <td valign=\"top\"> Demonstrate the modeling of heat losses.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Fittings.Bends</b></td></tr>
<tr><td valign=\"top\"> CurvedBend<br>
                      EdgedBend</td>
    <td valign=\"top\"> New fitting (pressure loss) components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Fittings.Orifices.</b></td></tr>
<tr><td valign=\"top\"> ThickEdgedOrifice</td>
    <td valign=\"top\"> New fitting (pressure loss) component.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Fittings.GenericResistances.</b></td></tr>
<tr><td valign=\"top\"> VolumeFlowRate</td>
    <td valign=\"top\"> New fitting (pressure loss) component.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math</b></td></tr>
<tr><td valign=\"top\"> isEqual </td>
    <td valign=\"top\"> Determine if two Real scalars are numerically identical.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Vectors</b></td></tr>
<tr><td valign=\"top\"> find </td>
    <td valign=\"top\"> Find element in vector.</td></tr>
<tr><td valign=\"top\"> toString </td>
    <td valign=\"top\"> Convert a real vector to a string.</td></tr>
<tr><td valign=\"top\"> interpolate </td>
    <td valign=\"top\"> Interpolate in a vector.</td></tr>
<tr><td valign=\"top\"> relNodePositions </td>
    <td valign=\"top\"> Return vector of relative node positions (0..1).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Vectors.Utilities</b></td></tr>
<tr><td valign=\"top\"> householderVector<br>
                      householderReflection<br>
                      roots </td>
    <td valign=\"top\"> Utility functions for vectors that are used by the newly introduced functions,
                      but are only of interested for a specialist.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Matrices</b></td></tr>
<tr><td valign=\"top\"> continuousRiccati<br>
                      discreteRiccati </td>
    <td valign=\"top\"> Return solution of continuous-time and discrete-time
                      algebraic Riccati equation respectively.</td></tr>
<tr><td valign=\"top\"> continuousSylvester<br>
                      discreteSylvester </td>
    <td valign=\"top\"> Return solution of continuous-time and discrete-time
                      Sylvester equation respectively.</td></tr>
<tr><td valign=\"top\"> continuousLyapunov<br>
                      discreteLyapunov </td>
    <td valign=\"top\"> Return solution of continuous-time and discrete-time
                      Lyapunov equation respectively.</td></tr>
<tr><td valign=\"top\"> trace </td>
    <td valign=\"top\"> Return the trace of a matrix.</td></tr>
<tr><td valign=\"top\"> conditionNumber </td>
    <td valign=\"top\"> Compute the condition number of a matrix.</td></tr>
<tr><td valign=\"top\"> rcond </td>
    <td valign=\"top\"> Estimate the reciprocal condition number of a matrix.</td></tr>
<tr><td valign=\"top\"> nullSpace </td>
    <td valign=\"top\"> Return a orthonormal basis for the null space of a matrix.</td></tr>
<tr><td valign=\"top\"> toString </td>
    <td valign=\"top\"> Convert a matrix into its string representation.</td></tr>
<tr><td valign=\"top\"> flipLeftRight </td>
    <td valign=\"top\"> Flip the columns of a matrix in left/right direction.</td></tr>
<tr><td valign=\"top\"> flipUpDown </td>
    <td valign=\"top\"> Flip the rows of a matrix in up/down direction.</td></tr>
<tr><td valign=\"top\"> cholesky </td>
    <td valign=\"top\"> Perform Cholesky factorization of a real symmetric positive definite matrix.</td></tr>
<tr><td valign=\"top\"> hessenberg </td>
    <td valign=\"top\"> Transform a matrix to upper Hessenberg form.</td></tr>
<tr><td valign=\"top\"> realSchur </td>
    <td valign=\"top\"> Computes the real Schur form of a matrix.</td></tr>
<tr><td valign=\"top\"> frobeniusNorm </td>
    <td valign=\"top\"> Return the Frobenius norm of a matrix.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.LAPACK.</b></td></tr>
<tr><td valign=\"top\"> dtrevc<br>
                      dpotrf<br>
                      dtrsm<br>
                      dgees<br>
                      dtrsen<br>
                      dgesvx<br>
                      dhseqr<br>
                      dlange<br>
                      dgecon<br>
                      dgehrd<br>
                      dgeqrf<br>
                      dggevx<br>
                      dgesdd<br>
                      dggev<br>
                      dggevx<br>
                      dhgeqz<br>
                      dormhr<br>
                      dormqr<br>
                      dorghr</td>
    <td valign=\"top\"> New interface functions for LAPACK
                      (should usually not directly be used but only indirectly via
                      Modelica.Math.Matrices).</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.Utilities.</b></td></tr>
<tr><td valign=\"top\"> reorderRSF<br>
                      continuousRiccatiIterative<br>
                      discreteRiccatiIterative<br>
                      eigenvaluesHessenberg<br>
                      toUpperHessenberg<br>
                      householderReflection<br>
                      householderSimilarityTransformation<br>
                      findLokal_tk</td>
    <td valign=\"top\"> Utility functions for matrices that are used by the newly introduced functions,
                      but are only of interested for a specialist.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Nonlinear</b></td></tr>
<tr><td valign=\"top\"> quadratureLobatto </td>
    <td valign=\"top\"> Return the integral of an integrand function using an adaptive Lobatto rule.</td></tr>
<tr><td valign=\"top\"> solveOneNonlinearEquation </td>
    <td valign=\"top\"> Solve f(u) = 0 in a very reliable and efficient way
                      (f(u_min) and f(u_max) must have different signs).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Nonlinear.Examples.</b></td></tr>
<tr><td valign=\"top\"> quadratureLobatto1<br>
                      quadratureLobatto2<br>
                      solveNonlinearEquations1<br>
                      solveNonlinearEquations2 </td>
    <td valign=\"top\"> Examples that demonstrate the usage of the Modelica.Math.Nonlinear functions
                      to integrate over functions and to solve scalar nonlinear equations.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.BooleanVectors.</b></td></tr>
<tr><td valign=\"top\"> allTrue </td>
    <td valign=\"top\"> Returns true, if all elements of the Boolean input vector are true.</td></tr>
<tr><td valign=\"top\"> anyTrue </td>
    <td valign=\"top\"> Returns true, if at least on element of the Boolean input vector is true.</td></tr>
<tr><td valign=\"top\"> oneTrue </td>
    <td valign=\"top\"> Returns true, if exactly one element of the Boolean input vector is true.</td></tr>
<tr><td valign=\"top\"> firstTrueIndex </td>
    <td valign=\"top\"> Returns the index of the first element of the Boolean vector that
                      is true and returns 0, if no element is true </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Icons.</b></td></tr>
<tr><td valign=\"top\"> Information<br>
                      Contact<br>
                      ReleaseNotes<br>
                      References<br>
                      ExamplesPackage<br>
                      Example<br>
                      Package<br>
                      BasesPackage<br>
                      VariantsPackage<br>
                      InterfacesPackage<br>
                      SourcesPackage<br>
                      SensorsPackage<br>
                      MaterialPropertiesPackage<br>
                      MaterialProperty </td>
    <td valign=\"top\"> New icons to get a unified view on different categories
                      of packages.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.SIunits.</b></td></tr>
<tr><td valign=\"top\"> ComplexCurrent<br>
                      ComplexCurrentSlope<br>
                      ComplexCurrentDensity<br>
                      ComplexElectricPotential<br>
                      ComplexPotentialDifference<br>
                      ComplexVoltage<br>
                      ComplexVoltageSlope<br>
                      ComplexElectricFieldStrength<br>
                      ComplexElectricFluxDensity<br>
                      ComplexElectricFlux<br>
                      ComplexMagneticFieldStrength<br>
                      ComplexMagneticPotential<br>
                      ComplexMagneticPotentialDifference<br>
                      ComplexMagnetomotiveForce<br>
                      ComplexMagneticFluxDensity<br>
                      ComplexMagneticFlux<br>
                      ComplexReluctance<br>
                      ComplexImpedance<br>
                      ComplexAdmittance<br>
                      ComplexPower</td>
    <td valign=\"top\"> SIunits to be used in physical models using complex variables, e.g.,<br>
                      <a href=\"modelica://Modelica.Electrical.QuasiStationary\">Modelica.Electrical.QuasiStationary</a>,
                      <a href=\"modelica://Modelica.Magnetic.FundamentalWave\">Modelica.Magnetic.FundamentalWave</a> </td></tr>
<tr><td valign=\"top\"> ImpulseFlowRate<br>
                      AngularImpulseFlowRate</td>
    <td valign=\"top\"> New SIunits for mechanics.</td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">existing components</b>
have been <b style=\"color:blue\">improved</b> in a
<b style=\"color:blue\">backward compatible</b> way:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.Blocks.Sources.</b></td></tr>
<tr><td valign=\"top\"> Pulse<br>
                      SawTooth </td>
    <td valign=\"top\"> New parameter \"nperiod\" introduced to define the number of periods
                      for the signal type. Default is \"infinite number of periods
                      (nperiods=-1).</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.</b></td></tr>
<tr><td valign=\"top\"> MultiPhase.*</td>
    <td valign=\"top\"> All dissipative components have now an optional heatPort connector
                      to which the dissipated losses are transported in form of heat.
                       </td></tr>
<tr><td valign=\"top\"> Machines.*</td>
    <td valign=\"top\"> To all electric machines (asynchronous and synchronous induction machines, DC machines)
                      and transformers loss models have been added (where applicable): <br>
                      Temperature dependent resistances (ohmic losses) <br>
                      Friction losses <br>
                      Brush losses <br>
                      Stray Load losses <br>
                      Core losses (only eddy current losses but no hysteresis losses; not for transformers) <br>
                      As default, temperature dependency and losses are set to zero. <br><br>
                      To all electric machines (asynchronous and synchronous induction machines, DC machines)
                      and transformers conditional thermal ports have been added,
                      to which the dissipated losses are flowing, if activated.
                      The thermal port contains a <a href=\"modelica://Modelica.Thermal.HeatTransfer.Interfaces.HeatPort\">HeatPort</a>
                      for each loss source of the specific machine type. <br><br>
                      To all electric machines (asynchronous and synchronous induction machines, DC machines)
                      a \"powerBalance\" result record has been added, summarizing converted power and losses.
                       </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.</b></td></tr>
<tr><td valign=\"top\"> MultiBody.*<br>
                      Rotational.*<br>
                      Translational.*</td>
    <td valign=\"top\"> All dissipative components in Modelica.Mechanics have now an
                      optional heatPort connector to which the dissipated energy is
                      transported in form of heat.<br>
                      All icons in Modelica.Mechanics are unified according to the
                      Modelica.Blocks library:<br>
                      \"%name\": width: -150 .. 150, height: 40, color: blue<br>
                      other text: height: 30, color: black
                       </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\"> World </td>
    <td valign=\"top\"> Function gravityAcceleration is made replaceable, so that redeclaration
                      yields user-defined gravity fields.
                       </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Valves.</b></td></tr>
<tr><td valign=\"top\"> ValveIncompressible<br>
                      ValveVaporizing<br>
                      ValveCompressible</td>
    <td valign=\"top\"> (a) Optional filtering of opening signal introduced to model
                      the delay time of the opening/closing drive. In this case, an optional
                      leakageOpening can be defined to model leakage flow and/or to
                      improve the numerics in certain situations.
                      (b) Improved regularization of the valve characteristics in some cases
                      so that it is twice differentiable (smooth=2),
                      instead of continuous (smooth=0).</td>
                      </tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Sources.</b></td></tr>
<tr><td valign=\"top\"> FixedBoundary<br>
                      Boundary_pT<br>
                      Boundary_ph</td>
    <td valign=\"top\"> Changed the implementation so that no non-linear algebraic
                      equation system occurs, if the given variables (e.g. p,T,X) do
                      not correspond to the medium states (e.g. p,h,X). This is
                      achieved by using appropriate \"setState_xxx\" calls to compute the
                      medium state from the given variables. If a nonlinear equation
                      system occurs, it is solved by a specialized handler inside the
                      setState_xxx(..) function, but in the model this equation system is
                      not visible.</td>
                      </tr>

<tr><td colspan=\"2\"><b>Modelica.Media.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialMedium </td>
    <td valign=\"top\"> The min/max values of types SpecificEnthalpy, SpecificEntropy,
                      SpecificHeatCapacity increased, due to reported user problems.<br>
                      New constant C_nominal introduced to provide nominal values for
                      trace substances (utilized in Modelica.Fluid to avoid numerical problems;
                      this fixes ticket
                      <a href=\"https://trac.modelica.org/Modelica/ticket/393\">#393</a>).</td>
                      </tr>

<tr><td colspan=\"2\"><b>Modelica.Thermal.</b></td></tr>
<tr><td valign=\"top\"> HeatTransfer.*</td>
    <td valign=\"top\"> All icons are unified according to the
                      Modelica.Blocks library:<br>
                      \"%name\": width: -150 .. 150, height: 40, color: blue<br>
                      other text: height: 30, color: black
                       </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Matrices</b></td></tr>
<tr><td valign=\"top\"> QR </td>
    <td valign=\"top\"> A Boolean input \"pivoting\" has been added (now QR(A, pivoting)) to provide QR-decomposition without pivoting (QR(A, false)). Default is pivoting=true.</td></tr>
</table>

<p><br>
The following <b style=\"color:red\">critical errors</b> have been fixed (i.e., errors
that can lead to wrong simulation results):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.Delay.</b></td></tr>
<tr><td valign=\"top\"> InertialDelaySensitive </td>
    <td valign=\"top\"> In order to decide whether the rising delay (tLH) or
                      the falling delay (tHL) is used, the \"previous\" value of the
                      output y has to be used and not the \"previous\" value of the
                      input x (delayType = delayTable[y_old, x] and not
                      delayType = delayTable[x_old, x]). This has been corrected.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Parts.</b></td></tr>
<tr><td valign=\"top\"> BodyBox<br>
                      BodyCylinder </td>
    <td valign=\"top\"> Fixes ticket
                      <a href=\"https://trac.modelica.org/Modelica/ticket/373\">#373</a>:
                      The \"Center of Mass\" was calculated as normalize(r)*length/2. This is
                      only correct if the box/cylinder is attached between frame_a and frame_b.
                      If this is not the case, the calculation is wrong.
                      The has been fixed by using the correct formula:<br>
                      r_shape + normalize(lengthDirection)*length/2</td></tr>
<tr><td valign=\"top\"> BodyShape<br>
                      BodyBox<br>
                      BodyCylinder </td>
    <td valign=\"top\"> Fixes ticket
                      <a href=\"https://trac.modelica.org/Modelica/ticket/300\">#300</a>:
                      If parameter enforceStates=true, an error occurred.
                      This has been fixed.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.Components.</b></td></tr>
<tr><td valign=\"top\"> LossyGear</td>
    <td valign=\"top\"> In cases where the driving flange is not obvious, the component could
                      lead to a non-convergent event iteration. This has been fixed
                      (a detailed description is provided in ticket
                      <a href=\"https://trac.modelica.org/Modelica/ticket/108\">#108</a>
                      and in the
                      <a href=\"modelica://Modelica/Resources/Documentation/Mechanics/Lossy-Gear-Bug_Solution.pdf\">attachment</a>
                      of this ticket).</td></tr>

<tr><td valign=\"top\"> Gearbox</td>
    <td valign=\"top\"> If useSupport=false, the support flange of the internal LossyGear
                      model was connected to the (disabled) support connector. As a result, the
                      LossyGear was \"free floating\". This has been corrected.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Pipes.</b></td></tr>
<tr><td valign=\"top\"> DynamicPipe</td>
    <td valign=\"top\"> Bug fix for dynamic mass, energy and momentum balances
                      for pipes with nParallel&gt;1.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Fluid.Pipes.BaseClasses.HeatTransfer.</b></td></tr>
<tr><td valign=\"top\"> PartialPipeFlowHeatTransfer</td>
    <td valign=\"top\"> Calculation of Reynolds numbers for the heat transfer through
                      walls corrected, if nParallel&gt;1.
                      This partial model is used by LocalPipeFlowHeatTransfer
                      for laminar and turbulent forced convection in pipes.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Media.Interfaces.PartialLinearFluid</b></td></tr>
<tr><td valign=\"top\"> setState_psX</td>
    <td valign=\"top\"> Sign error fixed.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Media.CompressibleLiquids.</b></td></tr>
<tr><td valign=\"top\"> LinearColdWater</td>
    <td valign=\"top\"> Fixed wrong values for thermal conductivity and viscosity.</td></tr>

</table>

<p><br>
The following <b style=\"color:red\">uncritical errors</b> have been fixed (i.e., errors
that do <b style=\"color:red\">not</b> lead to wrong simulation results, but, e.g.,
units are wrong or errors in documentation):
</p>
<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.LAPACK</b></td></tr>
<tr><td valign=\"top\"> dgesv_vec<br>
                        dgesv<br>
                        dgetrs<br>
                        dgetrf<br>
                        dgetrs_vec<br>
                        dgetri<br>
                        dgeqpf<br>
                        dorgqr<br>
                        dgesvx<br>
                        dtrsyl</td>
    <td valign=\"top\"> Integer inputs to specify leading dimensions of matrices have got a lower bound 1 (e.g., lda=max(1,n))
                      to avoid incorrect values (e.g., lda=0) in the case of empty matrices.<br>
                      The Integer variable \"info\" to indicate the successful call of a LAPACK routine has been converted to an output where it had been a protected variable.</td></tr>
</table>

<p><br>
The following
<a href=\"http://trac.modelica.org/Modelica\">trac tickets</a>
have been fixed:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica</b></td></tr>
<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/155\">#155</a></td>
    <td valign=\"top\">Wrong usage of \"fillColor\" and \"fillPattern\" annotations for lines</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/211\">#211</a></td>
    <td valign=\"top\">Undefined function realString used in MSL</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/216\">#216</a></td>
    <td valign=\"top\">Make MSL version 3.2 more Modelica 3.1 conform</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/218\">#218</a></td>
    <td valign=\"top\">Replace `Modelica://`-URIs by `modelica://`-URIs</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/271\">#271</a></td>
    <td valign=\"top\">Documentation URI errors in MSL 3.1</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/292\">#292</a></td>
    <td valign=\"top\">Remove empty \"\" annotations\"</td>
</tr>
<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/294\">#294</a></td>
    <td valign=\"top\">Typo 'w.r.t' --> 'w.r.t.'</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/296\">#296</a></td>
    <td valign=\"top\">Unify disclaimer message and improve bad style \"here\" links</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/333\">#333</a></td>
    <td valign=\"top\">Fix real number formats of the form `.[0-9]+`</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/347\">#347</a></td>
    <td valign=\"top\">invalid URI in MSL 3.2</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/355\">#355</a></td>
    <td valign=\"top\">Non-standard annotations</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Blocks</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/227\">#227</a></td>
    <td valign=\"top\">Enhance unit deduction functionality by adding 'unit=\"1\"' to some blocks\"</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/349\">#349</a></td>
    <td valign=\"top\">Incorrect annotation in Blocks/Continuous.mo</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/374\">#374</a></td>
    <td valign=\"top\">Parameter with no value at all in Modelica.Blocks.Continuous.TransferFunction</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Constants</b></td></tr>
<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/356\">#356</a></td>
    <td valign=\"top\">Add Euler-Mascheroni constant to Modelica.Constants</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Electrical.Analog</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/346\">#346</a></td>
    <td valign=\"top\">Multiple text in Modelica.Electrical.Analog.Basic.Conductor</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/363\">#363</a></td>
    <td valign=\"top\">Mixture of Real and Integer in index expressions in Modelica.Electrical.Analog.Lines</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/384\">#384</a></td>
    <td valign=\"top\">Incomplete annotations in some examples</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/396\">#396</a></td>
    <td valign=\"top\">Bug in Modelica.Electrical.Analog.Ideal.ControlledIdealIntermediateSwitch</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Machines</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/276\">#276</a></td>
    <td valign=\"top\">Improve/fix documentation of Modelica.Electrical.Machines</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/288\">#288</a></td>
    <td valign=\"top\">Describe thermal concept of machines</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/301\">#301</a></td>
    <td valign=\"top\">Documentation of Electrical.Machines.Examples needs update</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/306\">#306</a></td>
    <td valign=\"top\">Merge content of `Modelica.Electrical.Machines.Icons` into `Modelica.Icons`</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/362\">#362</a></td>
    <td valign=\"top\">Incomplete example model for DC machines</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/375\">#375</a></td>
    <td valign=\"top\">Strangeness with final parameters with no value but a start value</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Electrical.MultiPhase</b></td></tr>
<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/173\">#173</a></td>
    <td valign=\"top\">m-phase mutual inductor</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/200\">#200</a></td>
    <td valign=\"top\">adjust Multiphase to Analog</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/277\">#277</a></td>
    <td valign=\"top\">Improve/fix documentation of Modelica.Electrical.Multiphase</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/352\">#352</a></td>
    <td valign=\"top\">Odd annotation in Modelica.Electrical.MultiPhase.Sources.SignalVoltage</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Fluid</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/215\">#215</a></td>
    <td valign=\"top\">Bug in Modelica.Fluid.Pipes.DynamicPipe</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/219\">#219</a></td>
    <td valign=\"top\">Fluid.Examples.HeatExchanger: Heat transfer is switched off and cannot be enabled</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Math</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/348\">#348</a></td>
    <td valign=\"top\">Small error in documentation</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/371\">#371</a></td>
    <td valign=\"top\">Modelica.Math functions declared as \"C\" not \"builtin\"\"</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Mechanics.MultiBody</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/50\">#50</a></td>
    <td valign=\"top\">Error in LineForce handling of potential root</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/71\">#71</a></td>
    <td valign=\"top\">Make MultiBody.World replaceable</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/181\">#181</a></td>
    <td valign=\"top\">3d surface visualisation</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/210\">#210</a></td>
    <td valign=\"top\">Description of internal gear wheel missing</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/242\">#242</a></td>
    <td valign=\"top\">Missing each qualifier for modifiers in MultiBody.</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/251\">#251</a></td>
    <td valign=\"top\">Using enforceStates=true for BodyShape causes errors</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/255\">#255</a></td>
    <td valign=\"top\">Error in Revolute's handling of non-normalized axis of rotations</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/268\">#268</a></td>
    <td valign=\"top\">Non-standard annotation in MultiBody,Examples.Systems.RobotR3</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/269\">#269</a></td>
    <td valign=\"top\">What is the purpose of MultiBody.Examples.Systems.RobotR3.Components.InternalConnectors?</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/272\">#272</a></td>
    <td valign=\"top\">Function World.gravityAcceleration should not be protected</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/274\">#274</a></td>
    <td valign=\"top\">Convenient and mighty  initialization of frame kinematics</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/286\">#286</a></td>
    <td valign=\"top\">Typo in Multibody/Frames.mo</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/300\">#300</a></td>
    <td valign=\"top\">enforceStates parameter managed incorrectly in BodyShape, BodyBox, BodyCylinder</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/320\">#320</a></td>
    <td valign=\"top\">Replace non-standard annotation by `showStartAttribute`</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/373\">#373</a></td>
    <td valign=\"top\">Error in Modelica Mechanics</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/389\">#389</a></td>
    <td valign=\"top\">Shape.rxvisobj wrongly referenced in Arrow/DoubleArrow</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Mechanics.Rotational</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/108\">#108</a></td>
    <td valign=\"top\">Problem with model \"Lossy Gear\" and approach to a solution</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/278\">#278</a></td>
    <td valign=\"top\">Improve/fix documentation of Modelica.Mechanics.Rotational</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/381\">#381</a></td>
    <td valign=\"top\">Bug in Modelica.Mechanics.Rotational.Gearbox</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Mechanics.Translational</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/279\">#279</a></td>
    <td valign=\"top\">Improve/fix documentation of Modelica.Mechanics.Translational</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/310\">#310</a></td>
    <td valign=\"top\">Erroneous image links in `Modelica.Mechanics.Translational`</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Media</b></td></tr>
<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/72\">#72</a></td>
    <td valign=\"top\">PartialMedium functions not provided for all media in  Modelica.Media</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/217\">#217</a></td>
    <td valign=\"top\">Missing image file Air.png</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/224\">#224</a></td>
    <td valign=\"top\">dpT calculation in waterBaseProp_dT</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/393\">#393</a></td>
    <td valign=\"top\">Provide C_nominal in Modelica.Media to allow propagating
                     value and avoid wrong numerical results</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.StateGraph</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/206\">#206</a></td>
    <td valign=\"top\">Syntax error in StateGraph.mo</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/261\">#261</a></td>
    <td valign=\"top\">Modelica.StateGraph should mention the availability of Modelica_StateGraph2</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/354\">#354</a></td>
    <td valign=\"top\">Bad annotation in Modelica.StateGraph.Temporary.NumericValue</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Thermal.FluidHeatFlow</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/280\">#280</a></td>
    <td valign=\"top\">Improve/fix documentation of Modelica.Thermal.FluidHeatFlow</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Thermal.HeatTransfer</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/281\">#281</a></td>
    <td valign=\"top\">Improve/fix documentation of Modelica.Thermal.HeatTransfer</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.UsersGuide</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/198\">#198</a></td>
    <td valign=\"top\">Name of components in MSL not according to naming conventions</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/204\">#204</a></td>
    <td valign=\"top\">Minor correction to User's Guide's section on version management</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/244\">#244</a></td>
    <td valign=\"top\">Update the contacts section of the User's Guide</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/267\">#267</a></td>
    <td valign=\"top\">MSL-Documentation: Shouldn't equations be numbered on the right hand side?</td>
</tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/299\">#299</a></td>
    <td valign=\"top\">SVN keyword expansion messed up the User's guide section on version management</td>
</tr>

<tr><td colspan=\"2\"><br><b>Modelica.Utilities</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/249\">#249</a></td>
    <td valign=\"top\">Documentation error in ModelicaUtilities.h</td>
</tr>

<tr><td colspan=\"2\"><br><b>ModelicaServices</b></td></tr>

<tr><td valign=\"top\">
    <a href=\"https://trac.modelica.org/Modelica/ticket/248\">#248</a></td>
    <td valign=\"top\">No uses statement on ModelicaServices in MSL 3.1</td>
</tr>

</table>

<p>
Note:
</p>
<ul>
<li> Libraries
     <a href=\"https://www.modelica.org/libraries/Modelica_FundamentalWave\">Modelica_FundamentalWave</a>
     and
     <a href=\"https://www.modelica.org/libraries/Modelica_QuasiStationary\">Modelica_QuasiStationary</a>
     are included in this version in an improved form.</li>
<li> From library
     <a href=\"https://www.modelica.org/libraries/Modelica_LinearSystems2\">Modelica_LinearSystems2</a>,
     the sublibraries
     Math.Complex, Math.Vectors and Math.Matrices are included in this version
     in an improved form.</li>
<li> From library
     <a href=\"https://www.modelica.org/libraries/Modelica_StateGraph2\">Modelica_StateGraph2</a>,
     the sublibrary Blocks is included in this version in an improved form.</li>
</ul>
</html>"));
end Version_3_2;

class Version_3_1 "Version 3.1 (August 14, 2009)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p>
Version 3.1 is backward compatible to version 3.0 and 3.0.1,
i.e., models developed with version 3.0 or 3.0.1 will work without any
changes also with version 3.1.
</p>

<p>
Version 3.1 is slightly based on the Modelica Specification 3.1. It uses
the following new language elements (compared to Modelica Specification 3.0):
</p>

<ul>
<li> Prefix <u>stream</u> and built-in operators <u>inStream(..)</u>
     and <u>actualStream(..)</u> in Modelica.Fluid.</li>
<li> Annotation <u>connectorSizing</u> in Modelica.Fluid.</li>
<li> Annotation <u>inverse</u> in Modelica.Media.</li>
<li> Annotations <u>versionBuild</u>, <u>dateModified</u>,
     <u>revisionId</u> at the root level annotation of package Modelica,
     to improve the version handling.</li>
<li> Modifiers can be used in connectors instances (so balanced models
     are less restrictive). This allowed to make the implementation
     of conditional connectors (support and heatPort) in the Rotational,
     Translational and Electrical libraries simpler.</li>
</ul>

<p>
The following <b style=\"color:blue\">new libraries</b> have been added:
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Fluid\">Modelica.Fluid</a></td>
    <td valign=\"top\">
     Components to model 1-dim. thermo-fluid flow in networks of vessels, pipes,
     fluid machines, valves and fittings. All media from the
     Modelica.Media library can be used (so incompressible or compressible,
     single or multiple substance, one or two phase medium).
    The library is using the stream-concept from Modelica Specification 3.1.
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Magnetic.FluxTubes\">Modelica.Magnetic.FluxTubes</a></td>
    <td valign=\"top\">
     Components to model magnetic devices based on the magnetic flux tubes concepts.
     Especially to model
     electro-magnetic actuators. Nonlinear shape, force, leakage, and
     Material models. Material data for steel, electric sheet, pure iron,
     Cobalt iron, Nickel iron, NdFeB, Sm2Co17, and more.
    </td></tr>

<tr><td valign=\"top\"><a href=\"modelica://ModelicaServices\">ModelicaServices</a></td>
    <td valign=\"top\">
     New top level package that shall contain functions and models to be used in the
     Modelica Standard Library that requires a tool specific implementation.
     ModelicaServices is then used in the Modelica package.
     In this first version, the 3-dim. animation with model Modelica.Mechanics.MultiBody.Visualizers.Advanced.Shape
     was moved to ModelicaServices. Tool vendors can now provide their own implementation
     of the animation.
    </td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.</b></td></tr>
<tr><td valign=\"top\"> versionBuild<br>versionDate<br>dateModified<br>revisionId </td>
    <td valign=\"top\"> New annotations from Modelica 3.1 for version handling added.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.UsersGuide.ReleaseNotes.</b></td></tr>
<tr><td valign=\"top\"> VersionManagement </td>
    <td valign=\"top\"> Copied from info layer of previous ReleaseNotes (to make it more
                      visible) and adapted it to the new possibilities in
                      Modelica Specification 3.1.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Math.</b></td></tr>
<tr><td valign=\"top\"> RectangularToPolar<br>
                      PolarToRectangular </td>
    <td valign=\"top\"> New blocks to convert between rectangular and polar form
                      of space phasors.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Routing.</b></td></tr>
<tr><td valign=\"top\"> Replicator </td>
    <td valign=\"top\"> New block to replicate an input signal to many output signals.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Examples.</b></td></tr>
<tr><td valign=\"top\"> AmplifierWithOpAmpDetailed<br>
                      HeatingResistor<br>
                      CompareTransformers<br>
                      OvervoltageProtection<br>
                      ControlledSwitchWithArc<br>
                      SwitchWithArc<br>
                      ThyristorBehaviourTest</td>
    <td valign=\"top\"> New examples to demonstrate the usage of new components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Basic.</b></td></tr>
<tr><td valign=\"top\"> OpAmpDetailed<br>
                      TranslationalEMF<br>
                      M_Transformer</td>
    <td valign=\"top\"> New detailed model of an operational amplifier. <br>
                      New electromotoric force from electrical energy into mechanical translational energy.<br>
                      Generic transformer with choosable number of inductors</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Ideal.</b></td></tr>
<tr><td valign=\"top\"> OpenerWithArc<br>
                      CloserWithArc<br>
                      ControlledOpenerWithArc<br>
                      ControlledCloserWithArc</td>
    <td valign=\"top\"> New switches with simple arc model.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> ConditionalHeatPort</td>
    <td valign=\"top\"> New partial model to add a conditional HeatPort to
                      an electrical component.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Lines.</b></td></tr>
<tr><td valign=\"top\"> M_Oline</td>
    <td valign=\"top\"> New multiple line model, both the number of lines and the number of segments choosable.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Semiconductors.</b></td></tr>
<tr><td valign=\"top\"> ZDiode<br>Thyristor</td>
    <td valign=\"top\"> Zener Diode with 3 working areas and simple thyristor model.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Ideal.</b></td></tr>
<tr><td valign=\"top\"> OpenerWithArc<br>CloserWithArc</td>
    <td valign=\"top\"> New switches with simple arc model (as in Modelica.Electrical.Analog.Ideal.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Examples.Elementary.</b></td></tr>
<tr><td valign=\"top\"> RollingWheel<br>
                      RollingWheelSetDriving<br>
                      RollingWheelSetPulling</td>
    <td valign=\"top\"> New examples to demonstrate the usage of new components.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Joints.</b></td></tr>
<tr><td valign=\"top\"> RollingWheel<br>
                      RollingWheelSet</td>
    <td valign=\"top\"> New joints (no mass, no inertia) that describe an
                      ideal rolling wheel and a ideal rolling wheel set consisting
                      of two wheels rolling on the plane z=0.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Parts.</b></td></tr>
<tr><td valign=\"top\"> RollingWheel<br>
                      RollingWheelSet</td>
    <td valign=\"top\"> New ideal rolling wheel and ideal rolling wheel set consisting
                      of two wheels rolling on the plane z=0.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.</b></td></tr>
<tr><td valign=\"top\"> Ground</td>
    <td valign=\"top\"> New model to visualize the ground (box at z=0).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialElementaryOneFlangeAndSupport2<br>
                      PartialElementaryTwoFlangesAndSupport2</td>
    <td valign=\"top\"> New partial model with one and two flanges and the support flange
                      with a much simpler implementation as previously.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Translational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialElementaryOneFlangeAndSupport2<br>
                      PartialElementaryTwoFlangesAndSupport2</td>
    <td valign=\"top\"> New partial model with one and two flanges and the support flange
                      with a much simpler implementation as previously.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Media.IdealGases.Common.MixtureGasNasa.</b></td></tr>
<tr><td valign=\"top\"> setSmoothState</td>
    <td valign=\"top\"> Return thermodynamic state so that it smoothly approximates:
                      if x &gt; 0 then state_a else state_b.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Utilities.Internal.</b></td></tr>
<tr><td valign=\"top\"> PartialModelicaServices</td>
    <td valign=\"top\"> New package containing the interface description of
                      models and functions that require a tool dependent
                      implementation (currently only \"Shape\" for 3-dim. animation,
                      but will be extended in the future)</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Thermal.HeatTransfer.Components.</b></td></tr>
<tr><td valign=\"top\"> ThermalCollector</td>
    <td valign=\"top\"> New auxiliary model to collect the heat flows
                      from m heatports to a single heatport;
                      useful for multiphase resistors (with heatports)
                      as a junction of the m heatports.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Icons.</b></td></tr>
<tr><td valign=\"top\"> VariantLibrary<br>
                      BaseClassLibrary<br>
                      ObsoleteModel</td>
    <td valign=\"top\"> New icons (VariantLibrary and BaseClassLibrary have been moved
                      from Modelica_Fluid.Icons to this place).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.SIunits.</b></td></tr>
<tr><td valign=\"top\"> ElectricalForceConstant </td>
    <td valign=\"top\"> New type added (#190).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.SIunits.Conversions.</b></td></tr>
<tr><td valign=\"top\"> from_Hz<br>
                      to_Hz</td>
    <td valign=\"top\"> New functions to convert between frequency [Hz] and
                      angular velocity [1/s]. (#156) </td></tr>

</table>

<p><br>
The following <b style=\"color:blue\">existing components</b>
have been <b style=\"color:blue\">improved</b> in a
<b style=\"color:blue\">backward compatible</b> way:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.</b></td></tr>
<tr><td valign=\"top\"> Blocks<br>Mechanics<br>StateGraph </td>
    <td valign=\"top\"> Provided missing parameter values for examples
                      (these parameters had only start values)</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Basic</b></td></tr>
<tr><td valign=\"top\"> Resistor, Conductor, VariableResistor, VariableConductor</td>
    <td valign=\"top\"> Conditional heatport added for coupling to thermal network.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Ideal</b></td></tr>
<tr><td valign=\"top\"> Thyristors, Switches, IdealDiode</td>
    <td valign=\"top\"> Conditional heatport added for coupling to thermal network.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Semiconductors</b></td></tr>
<tr><td valign=\"top\"> Diode, ZDiode, PMOS, NMOS, NPN, PNP</td>
    <td valign=\"top\"> Conditional heatport added for coupling to thermal network.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Basic</b></td></tr>
<tr><td valign=\"top\"> Resistor, Conductor, VariableResistor, VariableConductor</td>
    <td valign=\"top\"> Conditional heatport added for coupling to thermal network (as in Modelica.Electrical.Analog).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase.Ideal</b></td></tr>
<tr><td valign=\"top\"> Thyristors, Switches, IdealDiode</td>
    <td valign=\"top\"> Conditional heatport added for coupling to thermal network (as in Modelica.Electrical.Analog).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Visualizers.Advanced.</b></td></tr>
<tr><td valign=\"top\"> Shape </td>
    <td valign=\"top\"> New implementation by inheriting from ModelicaServices. This allows a
                      tool vendor to provide its own implementation of Shape.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.StateGraph.</b></td></tr>
<tr><td valign=\"top\"> Examples </td>
    <td valign=\"top\"> Introduced \"StateGraphRoot\" on the top level of all example models.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.StateGraph.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> StateGraphRoot<br>PartialCompositeStep<br>CompositeStepState </td>
    <td valign=\"top\"> Replaced the wrong Modelica code \"flow output Real xxx\"
                      by \"Real dummy; flow Real xxx;\".
                      As a side effect, several \"blocks\" had to be changed to \"models\".</td></tr>
<tr><td valign=\"top\"> PartialStep </td>
    <td valign=\"top\"> Changed model by packing the protected outer connector in to a model.
                      Otherwise, there might be differences in the sign of the flow variable
                      in Modelica 3.0 and 3.1.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Utilities.Examples.</b></td></tr>
<tr><td valign=\"top\"> expression </td>
    <td valign=\"top\"> Changed local variable \"operator\" to \"opString\" since \"operator\"
                      is a reserved keyword in Modelica 3.1 </td></tr>
</table>

<p><br>
The following <b style=\"color:red\">uncritical errors</b> have been fixed (i.e., errors
that do <b style=\"color:red\">not</b> lead to wrong simulation results, but, e.g.,
units are wrong or errors in documentation):
</p>
<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Modelica.</b></td></tr>
<tr><td valign=\"top\"> Many models</td>
    <td valign=\"top\"> Removed wrong usages of annotations fillColor and fillPattern
                      in text annotations (#155, #185).</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines</b></td></tr>
<tr><td valign=\"top\"> All machine models</td>
    <td valign=\"top\"> The conditional heatports of the instantiated resistors
                        (which are new in Modelica.Electrical.Analog and Modelica.Electrical.MultiPhase)
                        are finally switched off until a thermal connector design for machines is implemented.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Media.Air.MoistAir</b></td></tr>
<tr><td valign=\"top\"> saturationPressureLiquid<br>
                      sublimationPressureIce<br>
                      saturationPressure</td>
          <td valign=\"top\"> For these three functions, an error in the <code>derivative</code> annotation was corrected. However, the effect of
                            this bug was minor, as a Modelica tool was allowed to compute derivatives automatically via
                            the <code>smoothOrder</code> annotation.</td>
</tr>
<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.</b></td></tr>
<tr><td valign=\"top\"> eigenValues</td>
    <td valign=\"top\"> Wrong documentation corrected (#162)</td></tr>
</table>

</html>"));
end Version_3_1;

class Version_3_0_1 "Version 3.0.1 (Jan. 27, 2009)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p>
This Modelica package is provided under the
<a href=\"modelica://Modelica.UsersGuide.ModelicaLicense2\">Modelica License 2</a>
and no longer under Modelica License 1.1. There are the following reasons
why the Modelica Association changes from Modelica License 1.1 to this
new license text (note, the text below is not a legal interpretation of the
Modelica License 2. In case of a conflict, the language of the license shall prevail):
</p>

<ol>
<li> The rights of licensor and licensee are much more clearly defined. For example:
         <ul>
         <li> The licensed work (Original Work) can be used in unmodified form in
                  open source and commercial software (the licensee cannot change the
                  license and the work must be provided without fees)</li>
         <li> If a model component is copied out of a Modelica package under
                  Modelica License 2 and is modified in order to adapt it to the needs
                  of the modeler, then the result can be licensed under any license
                  (including a commercial license).</li>
         <li> It is practically not possible to change the license of a
                  Modelica package under Modelica License 2 to another license, i.e., a
                  licensee cannot change the license by adding material or changing classes,
                  so the work must remain under Modelica License 2 (to be more precise,
                  if the licensee makes modifications to the Original Work \"which represents,
                  as a whole, an original work of authorship\", he/she can change the license
                  to another license. However, for a Modelica package this would
                  require a lot of changes which is usually unrealistic).</li>
         <li> If an executable is constructed using a Modelica package under
                  Modelica License 2, then this executable can be licensed under any
                  license (including a commercial license).</li>
         </ul>
         We hope that this compromise between open source contributors, commercial
         Modelica environments and Modelica users will motivate even more people to
         provide their Modelica packages freely under the Modelica License 2.<br><br></li>
<li> There are several new provisions that shall make law suites against licensors and licensees more unlikely (so the small risk is further reduced).</li>
</ol>

<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Electrical.Analog.Basic.</b></td></tr>
<tr><td valign=\"top\">M_Transformer</td>
          <td valign=\"top\"> Transformer, with the possibility to
        choose the number of inductors. The inductances and the coupled inductances
        can be chosen arbitrarily.</td></tr>

<tr><td colspan=\"2\"><b>Electrical.Analog.Lines.</b></td></tr>
<tr><td valign=\"top\">M_OLine</td>
          <td valign=\"top\"> Segmented line model that enables the use of
        multiple lines, that means, the number of segments and the number of
        single lines can be chosen by the user. The model allows to investigate
        phenomena at multiple lines like mutual magnetic or capacitive influence.</td></tr>
<tr><td colspan=\"2\"><b>Mechanics.Translational.Components.Examples.</b></td></tr>
<tr><td valign=\"top\">Brake</td>
          <td valign=\"top\"> Demonstrates the usage of the translational brake component.</td></tr>
<tr><td colspan=\"2\"><b>Media.Interfaces.PartialMedium.</b></td></tr>
<tr><td valign=\"top\">ThermoStates</td>
          <td valign=\"top\"> Enumeration type for independent variables to identify the independent
                                                variables of the medium (pT, ph, phX, pTX, dTX).<br>
                                                An implementation of this enumeration is provided for every medium.
                                                (This is useful for fluid libraries that do not use the
                                                PartialMedium.BaseProperties model).</td></tr>
<tr><td valign=\"top\">setSmoothState</td>
          <td valign=\"top\"> Function that returns the thermodynamic state which smoothly approximates:
                                                if x > 0 then state_a else state_b.<br>
                                                (This is useful for pressure drop components in fluid libraries
                                                 where the upstream density and/or viscosity has to be computed
                                                 and these properties should be smooth a zero mass flow rate)<br>
                                                An implementation of this function is provided for every medium.</td></tr>
<tr><td colspan=\"2\"><b>Media.Common.</b></td></tr>
<tr><td valign=\"top\">smoothStep</td>
          <td valign=\"top\"> Approximation of a general step, such that the characteristic
                                                is continuous and differentiable.</td></tr>
<tr><td colspan=\"2\"><b>Media.UsersGuide.</b></td></tr>
<tr><td valign=\"top\">Future</td>
          <td valign=\"top\"> Short description of goals and changes of upcoming release of Modelica.Media.</td></tr>
<tr><td colspan=\"2\"><b>Media.Media.Air.MoistAir.</b></td></tr>
<tr><td valign=\"top\">isentropicExponent</td>
          <td valign=\"top\"> Implemented Missing Function from interface.</td></tr>
<tr><td valign=\"top\">isentropicEnthalpyApproximation</td>
<td valign=\"top\"> Implemented function that approximates the isentropic enthalpy change.
This is only correct as long as there is no liquid in the stream.</td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">existing components</b>
have been <b style=\"color:blue\">changed</b> (in a
<b style=\"color:blue\">backward compatible</b> way):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Mechanics.Rotational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialFriction </td>
          <td valign=\"top\"> Improvement of friction model so that in certain situations
                                                the number of iterations is much smaller.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.Components.Examples.</b></td></tr>
<tr><td valign=\"top\"> Friction </td>
          <td valign=\"top\"> Added a third variant, where friction is modelled with
                                                the SupportFriction component.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.Components.</b></td></tr>
<tr><td valign=\"top\"> MassWithStopAndFriction </td>
          <td valign=\"top\"> Improvement of friction model so that in certain situations
                                                the number of iterations is much smaller.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialFriction </td>
          <td valign=\"top\"> Improvement of friction model so that in certain situations
                                                the number of iterations is much smaller.</td>
</tr>

<tr><td colspan=\"2\"><b>Media.Examples.</b></td></tr>
<tr><td valign=\"top\"> SimpleLiquidWater <br>
                                                IdealGasH20 <br>
                                                WaterIF97 <br>
                                                MixtureGases <br>
                                                MoistAir </td>
          <td valign=\"top\"> Added equations to test the new setSmoothState(..) functions
                                                including the analytic derivatives of these functions.</td></tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialLinearFluid.</b></td></tr>
<tr><td valign=\"top\"> setState_pTX <br>
                                                setState_phX <br>
                                                setState_psX <br>
                                                setState_dTX </td>
          <td valign=\"top\"> Rewritten function in one statement so that it is usually inlined.</td></tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialLinearFluid.</b></td></tr>
<tr><td valign=\"top\"> consistent use of reference_d instead of density(state </td>
          <td valign=\"top\"> Change was done to achieve consistency with analytic inverse functions.</td></tr>

<tr><td colspan=\"2\"><b>Media.Air.MoistAir.</b></td></tr>
<tr><td valign=\"top\"> T_phX </td>
          <td valign=\"top\"> Interval of nonlinear solver to compute T from p,h,X changed
                                                from 200..6000 to 240 ..400 K.</td></tr>

</table>

<p><br>
The following <b style=\"color:red\">critical errors</b> have been fixed (i.e., errors
that can lead to wrong simulation results):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Forces</b></td></tr>
<tr><td valign=\"top\"> WorldTorque </td>
          <td valign=\"top\"> Parameter \"ResolveInFrame\" was not propagated and therefore
                                                always the default (resolved in world frame) was used, independently
                                                of the setting of this parameter.</td>
</tr>
<tr><td valign=\"top\"> WorldForceAndTorque </td>
          <td valign=\"top\"> Parameter \"ResolveInFrame\" was not propagated and therefore
                                                always the default (resolved in world frame) was used, independently
                                                of the setting of this parameter.<br>
                                                Furthermore, internally WorldTorque was used instead of
                                                Internal.BasicWorldTorque and therefore the visualization of
                                                worldTorque was performed twice.</td>
</tr>
<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Sensors</b></td></tr>
<tr><td valign=\"top\"> AbsoluteSensor </td>
          <td valign=\"top\"> Velocity, acceleration and angular acceleration were computed
                                                  by differentiating in the resolveInFrame frame. This has been corrected, by
                                                  first transforming the vectors in to the world frame, differentiating here
                                                  and then transforming into resolveInFrame. The parameter in the Advanced menu
                                                  resolveInFrameAfterDifferentiation is then superfluous and was removed .</td>
</tr>
<tr><td valign=\"top\"> AbsoluteVelocity </td>
          <td valign=\"top\"> The velocity was computed
                                                  by differentiating in the resolveInFrame frame. This has been corrected, by
                                                  first transforming the velocity in to the world frame, differentiating here
                                                  and then transforming into resolveInFrame </td>
</tr>
<tr><td valign=\"top\"> RelativeSensor </td>
          <td valign=\"top\"> If resolveInFrame &lt;&gt; frame_resolve and
                                                   resolveInFrameAfterDifferentiation = frame_resolve, a translation
                                                error occurred, since frame_resolve was not enabled in this situation.
                                                This has been corrected.</td>
</tr>
<tr><td valign=\"top\"> RelativeVelocity </td>
          <td valign=\"top\"> The velocity has have been computed
                                                  by differentiating in the resolveInFrame frame. This has been corrected, by
                                                  first transforming the relative position in to frame_a, differentiating here
                                                  and then transforming into resolveInFrame </td>
</tr>
<tr><td valign=\"top\"> TransformRelativeVector </td>
          <td valign=\"top\"> The transformation was wrong, since the parameters frame_r_in and frame_r_out
                                                have not been propagated to the submodel that performs the transformation.
                                                This has been corrected.</td>
</tr>
<tr><td colspan=\"2\"><b>Mechanics.Translational.Components.</b></td></tr>
<tr><td valign=\"top\"> SupportFriction<br>
                                                Brake </td>
          <td valign=\"top\"> The sign of the friction force was wrong and therefore friction accelerated
                                                instead of decelerated. This was fixed.</td>
</tr>
<tr><td valign=\"top\"> SupportFriction</td>
          <td valign=\"top\"> The component was only correct for fixed support.
                                                This was corrected.</td>
</tr>
<tr><td colspan=\"2\"><b>Media.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialSimpleMedium<br>
                                                PartialSimpleIdealGasMedium </td>
          <td valign=\"top\"> BaseProperties.p was not defined as preferred state and BaseProperties.T was
                                                always defined as preferred state. This has been fixed by
                                                Defining p,T as preferred state if parameter preferredMediumState = true.
                                                This error had the effect that mass m is selected as state instead of p
                                                and if default initialization is used then m=0 could give not the expected
                                                behavior. This means, simulation is not wrong but the numerics is not as good
                                                and if a model relies on default initial values, the result could be not
                                                as expected.</td>
</tr>

</table>

<p><br>
The following <b style=\"color:red\">uncritical errors</b> have been fixed (i.e., errors
that do <b style=\"color:red\">not</b> lead to wrong simulation results, but, e.g.,
units are wrong or errors in documentation):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.Math.</b></td></tr>
<tr><td valign=\"top\"> InverseBlockConstraint </td>
          <td valign=\"top\"> Changed annotation preserveAspectRatio from true to false.</td>
</tr>

<tr><td colspan=\"2\"><b>Blocks.Sources.</b></td></tr>
<tr><td valign=\"top\"> RealExpression<br>
                                                IntegerExpression<br>
                                                BooleanExpression </td>
          <td valign=\"top\"> Changed annotation preserveAspectRatio from true to false.</td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Analog.Basic.</b></td></tr>
<tr><td valign=\"top\"> SaturatingInductor</td>
          <td valign=\"top\"> Replaced non-standard \"arctan\" by \"atan\" function.</td>
</tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Digital.</b></td></tr>
<tr><td valign=\"top\"> UsersGuide</td>
          <td valign=\"top\"> Removed empty documentation placeholders and added the missing
                                                  release comment for version 1.0.7</td>
</tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Translational.Components.</b></td></tr>
<tr><td valign=\"top\"> MassWithStopAndFriction </td>
          <td valign=\"top\"> Changed usage of reinit(..), in order that it appears
                                                only once for one variable according to the language specification
                                                (if a tool could simulate the model, there is no difference).</td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialSimpleMedium</b></td></tr>
<tr><td valign=\"top\"> pressure<br>
                                                temperature<br>
                                                density<br>
                                                specificEnthalpy </td>
          <td valign=\"top\"> Missing functions added.</td>
</tr>

</table>

</html>"));
end Version_3_0_1;

class Version_3_0 "Version 3.0 (March 1, 2008)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>
<p>
Version 3.0 is <b>not</b> backward compatible to previous versions.
A conversion script is provided to transform models and libraries
of previous versions to the new version. Therefore, conversion
should be automatic.
</p>

<p>
The following changes are present for the whole library:
</p>

<ul>
<li> In the Modelica language version 3.0, several restrictions have been
         introduced to allow better checking, e.g., models on all levels must be balanced
         (number of equations = number of unknown variables - unknown variables that have
         to be defined when using the component). A few models of the Modelica
         Standard Library did not fulfill these new restrictions and had
         either to be moved to library ObsoleteModelica3 (e.g., Blocks.Math.TwoInputs)
         or had to be differently implemented
         (e.g., Media.Interfaces.PartialMedium.BaseProperties).
         The Modelica Standard Library version 3.0 fulfills all the restrictions of
         the Modelica Language version 3.0.<br>&nbsp;
         </li>

<li> The graphical annotations describing the layout of icon and diagram layer
         are changed from Modelica language version 1 to Modelica language version 3.
         This gives several significant improvements:<br>Especially, the coordinate systems
         of icon and diagram layers are no longer coupled and therefore the size of the
         icon layer can be changed independently of the size of the diagram layer.
         Also it can be defined that the aspect ratio of a component icon is kept when changing
         its size in a model. This flag is set so that all icons of the Modelica
         Standard Library keep its aspect ratios. This is slightly non-backward compatible:
         If the aspect ratio was not kept when using a component from the Modelica
         Standard Library, it is now resized so that the aspect ratio is maintained.<br>&nbsp; </li>

<li> All non-standard annotations removed by:<br>
         (1) Removing the annotation since without effect
                 (e.g., \"__Dymola_experimentSetupOutput\", \"Window\", \"Terminal\" removed).<br>
         (2) Renaming the annotation to a standard name (e.g., \"Hide\" renamed to \"HideResult\").<br>
         (3) Renaming the annotation to a vendor specific name
                 (e.g., \"checkBox\" renamed to \"__Dymola_checkBox\").<br>&nbsp; </li>

<li> All emulated enumerations (defined via packages and constants) have been
         replaced by \"real\" enumerations. User models are automatically correctly
         converted, provided the user models used the package constants previously.
         <b>Existing models that use directly literal values for enumerations, might give in
         some cases wrong results</b> (if the first constant of the emulated enumeration
         had value zero, whereas the first value of an enumeration is one).<br>&nbsp; </li>

<li> The operator \"cardinality\" will be removed in one of the next versions of the
         Modelica language, since it is a reflective operator and its usage significantly
         reduces the possibilities of advanced model checks (e.g., to guarantee that a model
         is \"balanced\", i.e., the number of equations and unknowns is identical,
         for all valid usages of the component). As a preparation for this change, all
         models that contain the \"cardinality(..)\" operator are rewritten: If possible
         the operator is removed. If this is not possible, it is only used in asserts to
         check that, e.g., a connector is connected at least once or is connected exactly
         once. In the next Modelica language version new language elements will be introduced
         to specify such a property check without the cardinality operator. Once these
         language elements are available, the cardinality operator will be removed completely
         from the Modelica Standard Library.<br>
         The changes with respect to the cardinality(..) operator are usually not backward
         compatible. This is the reason for the changes of the
         Rotational and Translational library (see below).<br>&nbsp;</li>

<li> The design of the <b>Rotational</b> and <b>Translational</b> libraries have been changed
         (especially to remove the cardinality(..) operator, see above):
         <ul>
         <li> Components have a <b>useSupport</b> flag to enable or disable a support flange.
                  If the support flange is enabled, it must be connected. If it is disabled, it must
                  not be connected and the component is then internally grounded. The grounding
                  is visualized in the icon.</li>
         <li> The relative angle/distance and the relative speed of all force/torque elements
                  (that need the relative speed) are by default defined with \"StateSelect.prefer\", i.e.,
                  to use these variables as preferred states. This improves the numerics if the
                  absolute angle or the absolute distance are continuously increasing during
                  operation (e.g., driving shaft of the wheels of a car). The effect is that relative
                  angles/distances and speeds are used as states and the size of these variables is
                  limited. Previously, the default was to use the absolute angle/distance
                  and absolute speed of every inertia/mass which has the disadvantage that the absolute
                  angle and or distance are state variables that grow in size continuously.<br>
                  A significant advantage is also, that default initialization is usually better,
                  because a default value of zero for a relative angle/distance is usually what the
                  user would like to have. Previously, say, the load was initialized to a non-zero
                  angle and then the elastically coupled motor inertia had to be explicitly
                  also initialized with this value. This is now, no longer needed. Since the default
                  nominal value of 1 is usually too large for a relative quantity, the nominal
                  values of the relative angle/distance was changed to 1e-4.</li>
         <li> The two libraries have been restructured in sublibraries to cope
                  with the growing number of components.</li>
         <li> Finally, the Translational library has been
                  made as similar as possible to the Rotational library by, e.g., adding missing
                  components.<br>&nbsp;</li>
         </ul></li>

<li> The initialization of the MultiBody, Rotational and Translational libraries have
         been significantly simplified by removing the \"initType\" parameters and only
         using start/fixed values. This design assumes that a tool has special support
         for start/fixed values in the parameter menu.<br>&nbsp;</li>

<li> Nearly all parameters defined in the Modelica Standard Library had been
         defined with a default equation, e.g.,
         <pre>   <b>parameter</b> Modelica.SIunits.Resistance R=1; </pre>
         Physical parameters, such as a resistance, mass, gear ratio, do not have a meaningful
         default and in nearly all cases, the user of the corresponding component has to
         provide values for such parameters. If the user forgets this, a tool
         cannot provide diagnostics, since a default value is present in the library
         (such as 1 Ohm for the resistance). In most cases the model will simulate but will
         give wrong results due to wrong parameter values. To improve this situation, all physical
         parameter declarations in the Modelica Standard Library have been changed, so
         that the previous default becomes a start value. For example, the above
         declaration is changed to:
         <pre>   <b>parameter</b> Modelica.SIunits.Resistance R(start=1);  </pre>
         This is a backward compatible change and completely equivalent from the perspective
         of the Modelica language. It is, however, advised that tools will print a warning
         or optionally an error message, if the start value of a parameter is defined, but
         no value for the parameter is given via a modification. Furthermore, it is expected,
         that the input field of a parameter menu is empty, if no default equation is defined,
         but only a start value. This shows clearly to the modeler that a value has to
         be provided.</li>
</ul>

<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries (note, the names in parentheses
are the new sublibrary names that are introduced in version 3.0):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.Examples.</b></td></tr>
<tr><td valign=\"top\">InverseModel</td>
          <td valign=\"top\"> Demonstrates the construction of an inverse model.</td></tr>

<tr><td colspan=\"2\"><b>Blocks.Math.</b></td></tr>
<tr><td valign=\"top\">InverseBlockConstraints</td>
          <td valign=\"top\"> Construct inverse model by requiring that two inputs
                                                and two outputs are identical (replaces the previously,
                                                unbalanced, TwoInputs and TwoOutputs blocks).</td></tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Utilities</b></td></tr>
<tr><td valign=\"top\">TransformerData</td>
          <td valign=\"top\"> A record that calculates required impedances (parameters) from nominal data of transformers.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Examples.Rotational3DEffects</b></td></tr>
<tr><td valign=\"top\"> GyroscopicEffects<br>
                                                ActuatedDrive<br>
                                                MovingActuatedDrive<br>
                                                GearConstraint </td>
          <td valign=\"top\"> New examples to demonstrate the usage of the Rotational library
                                                in combination with multi-body components.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Sensors</b></td></tr>
<tr><td valign=\"top\"> AbsolutePosition<br>
                                                AbsoluteVelocity<br>
                                                AbsoluteAngles<br>
                                                AbsoluteAngularVelocity<br>
                                                RelativePosition<br>
                                                RelativeVelocity<br>
                                                RelativeAngles<br>
                                                RelativeAngularVelocity</td>
          <td valign=\"top\"> New sensors to measure one vector.</td>
</tr>
<tr><td valign=\"top\"> TransformAbsoluteVector<br>
                                                TransformRelativeVector</td>
          <td valign=\"top\"> Transform absolute and/or relative vector into another frame.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.(Components)</b></td></tr>
<tr><td valign=\"top\"> Disc </td>
          <td valign=\"top\"> Right flange is rotated by a fixed angle with respect to left flange</td></tr>
<tr><td valign=\"top\"> IdealRollingWheel </td>
          <td valign=\"top\"> Simple 1-dim. model of an ideal rolling wheel without inertia</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.Sensors</b></td></tr>
<tr><td valign=\"top\">RelPositionSensor<br>RelSpeedSensor<br>RelAccSensor<br>PowerSensor</td>
          <td valign=\"top\"> Relative position sensor, i.e., distance between two flanges<br>
                                                Relative speed sensor<br>
                                                Relative acceleration sensor<br>
                                                Ideal power sensor</td></tr>
<tr><td colspan=\"2\"><b>Mechanics.Translational(.Components)</b></td></tr>
<tr><td valign=\"top\">SupportFriction<br>Brake<br>InitializeFlange</td>
          <td valign=\"top\"> Model of friction due to support<br>
                                                Model of a brake, base on Coulomb friction<br>
                                                Initializes a flange with pre-defined position, speed and acceleration .</td></tr>
<tr><td colspan=\"2\"><b>Mechanics.Translational(.Sources)</b></td></tr>
<tr><td valign=\"top\">Force2<br>LinearSpeedDependentForce<br>QuadraticSpeedDependentForce<br>
                                           ConstantForce<br>ConstantSpeed<br>ForceStep</td>
          <td valign=\"top\"> Force acting on 2 flanges<br>
                                                Force linearly dependent on flange speed<br>
                                                Force quadratic dependent on flange speed<br>
                                                Constant force source<br>
                                                Constant speed source<br>
                                                Force step</td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">existing components</b>
have been <b style=\"color:blue\">changed</b> in a
<b style=\"color:blue\">non-backward compatible</b> way
(the conversion script transforms models and libraries
of previous versions to the new version. Therefore, conversion
should be automatic):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.Continuous.</b></td></tr>
<tr><td valign=\"top\"> CriticalDamping </td>
          <td valign=\"top\"> New parameter \"normalized\" to define whether filter is provided
                                                in normalized or non-normalized form. Default is \"normalized = true\".
                                                The previous implementation was a non-normalized filter.
                                                The conversion script automatically introduces the modifier
                                                \"normalized=false\" for existing models.</td></tr>

<tr><td colspan=\"2\"><b>Blocks.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> RealInput<br>
                                                RealOutput</td>
          <td valign=\"top\"> Removed \"SignalType\", since extending from a replaceable class
                                                and this is not allowed in Modelica 3.<br>The conversion script
                                                removes modifiers to SignalType.</td></tr>

<tr><td valign=\"top\"> RealSignal<br>
                                                IntegerSignal<br>
                                                BooleanSignal</td>
          <td valign=\"top\"> Moved to library ObsoleteModelica3, since these connectors
                                                are no longer allowed in Modelica 3<br>
                                                (prefixes input and/or output are required).</td></tr>

<tr><td colspan=\"2\"><b>Blocks.Interfaces.Adaptors.</b></td></tr>
<tr><td valign=\"top\"> AdaptorReal<br>
                                                AdaptorBoolean<br>
                                                AdaptorInteger</td>
          <td valign=\"top\"> Moved to library ObsoleteModelica3, since the models are not \"balanced\".
                                                These are completely obsolete adaptors<br>between the Real, Boolean, Integer
                                                signal connectors of version 1.6 and version &ge; 2.1 of the Modelica
                                                Standard Library.</td></tr>

<tr><td colspan=\"2\"><b>Blocks.Math.</b></td></tr>
<tr><td valign=\"top\"> ConvertAllUnits</td>
          <td valign=\"top\"> Moved to library ObsoleteModelica3, since extending from a replaceable class
                                                and this is not allowed in Modelica 3.<br> It would be possible to rewrite this
                                                model to use a replaceable component. However, the information about the
                                                conversion<br> cannot be visualized in the icon in this case.</td></tr>

<tr><td colspan=\"2\"><b>Blocks.Math.UnitConversions.</b></td></tr>
<tr><td valign=\"top\"> TwoInputs<br>
                                                TwoOutputs</td>
          <td valign=\"top\"> Moved to library ObsoleteModelica3, since the models are not \"balanced\".
                                                A new component<br>\"InverseBlockConstraints\"
                                                is provided instead that has the same feature, but is \"balanced\".</td></tr>

<tr><td colspan=\"2\"><b>Electrical.Analog.Baisc.</b></td></tr>
<tr><td valign=\"top\"> HeatingResistor</td>
          <td valign=\"top\"> The heatPort has to be connected; otherwise the component Resistor (without heatPort) has to be used.<br>
                                                cardinality() is only used to check whether the heatPort is connected.</td></tr>

<tr><td colspan=\"2\"><b>Electrical.MultiPhase.Examples.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Changed the instance names of components used in the examples to more up-to-date style.</td></tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Moved package <code>Machines.Examples.Utilities</code> to <code>Machines.Utilities</code></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Removed all nonSIunits; especially in DCMachines<br>
                                                parameter NonSIunits.AngularVelocity_rpm rpmNominal was replaced by<br>
                                                parameter SIunits.AngularVelocity wNominal</td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Changed the following component variable and parameter names to be more concise:<br>
                                                Removed suffix \"DamperCage\" from all synchronous induction machines
                                                since the user can choose whether the damper cage is present or not.<br><code>
                                                RotorAngle ... RotorDisplacementAngle<br>
                                                J_Rotor ... Jr<br>
                                                Rr ........ Rrd (damper of synchronous induction machines)<br>
                                                Lrsigma ... Lrsigmad (damper of synchronous induction machines)<br>
                                                phi_mechanical ... phiMechanical<br>
                                                w_mechanical ..... wMechanical<br>
                                                rpm_mechanical ... rpmMechanical<br>
                                                tau_electrical ... tauElectrical<br>
                                                tau_shaft ........ tauShaft<br>
                                                TurnsRatio ....... turnsRatio    (AIMS)<br>
                                                VsNom ............ VsNominal     (AIMS)<br>
                                                Vr_Lr ............ VrLockedRotor (AIMS)<br>
                                                DamperCage ....... useDamperCage (synchronous induction machines)<br>
                                                V0 ............... VsOpenCicuit  (SMPM)<br>
                                                Ie0 .............. IeOpenCicuit  (SMEE)
                                                </code></td></tr>
<tr><td valign=\"top\">Interfaces.</td>
          <td valign=\"top\"> Moved as much code as possible from specific machine models to partials to reduce redundant code.</td></tr>
<tr><td valign=\"top\">Interfaces.Adapter</td>
          <td valign=\"top\"> Removed to avoid cardinality; instead, the following solution has been implemented:</td></tr>
<tr><td valign=\"top\">Sensors.RotorDisplacementAngle<br>Interfaces.PartialBasicMachine</td>
          <td valign=\"top\"> Introduced <code>parameter Boolean useSupport=false \"enable / disable (=fixed stator) support\"</code><br>
                                                The rotational support connector is only present with <code>useSupport = true;</code><br>
                                                otherwise the stator is fixed internally.</td></tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Examples.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Changed the names of the examples to more meaningful names.<br>
                                                Changed the instance names of components used in the examples to more up-to-date style.</td></tr>
<tr><td valign=\"top\">SMEE_Generator</td>
          <td valign=\"top\"> Initialization of <code>smee.phiMechanical</code> with <code>fixed=true</code></td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\"> World</td>
          <td valign=\"top\"> Changed default value of parameter driveTrainMechanics3D from false to true.<br>
                                                3-dim. effects in Rotor1D, Mounting1D and BevelGear1D are therefore taken<br>
                                                into account by default (previously this was only the case, if
                                                world.driveTrainMechanics3D was explicitly set).</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Forces.</b></td></tr>
<tr><td valign=\"top\"> FrameForce<br>
                                                FrameTorque<br>
                                                FrameForceAndTorque</td>
          <td valign=\"top\"> Models removed, since functionality now available via Force, Torque, ForceAndTorque</td></tr>
<tr><td valign=\"top\"> WorldForce<br>
                                                WorldTorque<br>
                                                WorldForceAndTorque<br>
                                                Force<br>
                                                Torque<br>
                                                ForceAndTorque</td>
          <td valign=\"top\"> Connector frame_resolve is optionally enabled via parameter resolveInFrame<br>.
                                                Forces and torques and be resolved in all meaningful frames defined
                                                by enumeration resolveInFrame.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Frames.</b></td></tr>
<tr><td valign=\"top\"> length<br>
                                                normalize</td>
          <td valign=\"top\"> Removed functions, since available also in Modelica.Math.Vectors
                                                <br>The conversion script changes the references correspondingly.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Joints.</b></td></tr>
<tr><td valign=\"top\"> Prismatic<br>
                                                ActuatedPrismatic<br>
                                                Revolute<br>
                                                ActuatedRevolute<br>
                                                Cylindrical<br>
                                                Universal<br>
                                                Planar<br>
                                                Spherical<br>
                                                FreeMotion</td>
          <td valign=\"top\"> Changed initialization, by replacing initial value parameters with
                                                start/fixed attributes.<br>
                                                When start/fixed attributes are properly supported
                                                in the parameter menu by a Modelica tool,<br>
                                                the initialization is considerably simplified for the
                                                user and the implementation is much simpler.<br>
                                                Replaced parameter \"enforceStates\" by the more general
                                                built-in enumeration stateSelect=StateSelection.xxx.<br>
                                                The conversion script automatically
                                                transforms from the \"old\" to the \"new\" forms.</td></tr>
<tr><td valign=\"top\"> Revolute<br>
                                                ActuatedRevolute</td>
          <td valign=\"top\"> Parameter \"planarCutJoint\" in the \"Advanced\" menu of \"Revolute\" and of
                                                \"ActuatedRevolute\" removed.<br>
                                                A new joint \"RevolutePlanarLoopConstraint\" introduced that defines the constraints
                                                of a revolute joint<br> as cut-joint in a planar loop.
                                                This change was needed in order that the revolute joint can be
                                                properly used<br>in advanced model checking.<br>
                                                ActuatedRevolute joint removed. Flange connectors of Revolute joint<br>
                                                can be enabled with parameter useAxisFlange.</td></tr>
<tr><td valign=\"top\"> Prismatic<br>
                                                ActuatedPrismatic</td>
          <td valign=\"top\"> ActuatedPrismatic joint removed. Flange connectors of Prismatic joint<br>
                                                can be enabled with parameter useAxisFlange.</td></tr>
<tr><td valign=\"top\"> Assemblies</td>
          <td valign=\"top\"> Assembly joint implementation slightly changed, so that
                                                annotation \"structurallyIncomplete\" <br>could be removed
                                                (all Assembly joint models are now \"balanced\").</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Joints.Internal</b></td></tr>
<tr><td valign=\"top\"> RevoluteWithLengthConstraint<br>
                                                PrismaticWithLengthConstraint</td>
          <td valign=\"top\"> These joints should not be used by a user of the MultiBody library.
                                                They are only provided to built-up the
                                                MultiBody.Joints.Assemblies.JointXYZ joints.
                                                These two joints have been changed in a slightly not backward compatible
                                                way, in order that the usage in the Assemblies.JointXYZ joints results in
                                                balanced models (<b>no conversion is provided for this change since the
                                                user should not have used these joints and the conversion would be too
                                                complicated</b>):
                                                In releases before version 3.0 of the Modelica Standard Library,
                                                it was possible to activate the torque/force projection equation
                                                (= cut-torque/-force projected to the rotation/translation
                                                axis must be identical to
                                                the drive torque/force of flange axis) via parameter <b>axisTorqueBalance</b>.
                                                This is no longer possible, since otherwise this model would not be
                                                \"balanced\" (= same number of unknowns as equations). Instead, when
                                                using this model in version 3.0 and later versions, the torque/force
                                                projection equation must be provided in the Advanced menu of joints
                                                Joints.SphericalSpherical and Joints.UniversalSpherical
                                                via the new parameter \"constraintResidue\".</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Parts.</b></td></tr>
<tr><td valign=\"top\"> BodyBox<br>
                                                BodyCylinder</td>
          <td valign=\"top\"> Changed unit of parameter density from g/cm3 to the SI unit kg/m3
                                                in order to allow stricter unit checking.<br>The conversion script multiplies
                                                previous density values with 1000.</td></tr>
<tr><td valign=\"top\"> Body<br>
                                                BodyShape<br>
                                                BodyBox<br>
                                                BodyCylinder<br>
                                                PointMass
                                                Rotor1D</td>
          <td valign=\"top\"> Changed initialization, by replacing initial value parameters with
                                                start/fixed attributes.<br>
                                                When start/fixed attributes are properly supported
                                                in the parameter menu by a Modelica tool,<br>
                                                the initialization is considerably simplified for the
                                                user and the implementation is much simpler.<br>The conversion script automatically
                                                transforms from the \"old\" to the \"new\" form of initialization.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Sensors.</b></td></tr>
<tr><td valign=\"top\"> AbsoluteSensor<br>
                                                RelativeSensor<br>
                                                CutForceAndTorque</td>
          <td valign=\"top\"> New design of sensor components: Via Boolean parameters<br>
                                                signal connectors for the respective vectors are enabled/disabled.<br>
                                                It is not possible to automatically convert models to this new design.<br>
                                                Instead, references in existing models are changed to ObsoleteModelice3.<br>
                                                This means that these models must be manually adapted.</td></tr>
<tr><td valign=\"top\"> CutForce<br>
                                                CutTorque</td>
          <td valign=\"top\"> Slightly new design. The force and/or torque component can be
                                                resolved in world, frame_a, or frame_resolved.<br>
                                                Existing models are automatically converted.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Moved components to structured sub-packages (Sources, Components)</td></tr>
<tr><td valign=\"top\"> Inertia<br>
                                                SpringDamper<br>
                                                RelativeStates</td>
          <td valign=\"top\"> Changed initialization, by replacing initial value parameters with
                                                start/fixed attributes.<br>
                                                When start/fixed attributes are properly supported
                                                in the parameter menu by a Modelica tool,<br>
                                                the initialization is considerably simplified for the
                                                user and the implementation is much simpler.<br>
                                                Parameter \"stateSelection\" in \"Inertia\" and \"SpringDamper\" replaced
                                                by the built-in enumeration<br>stateSelect=StateSelection.xxx.
                                                Introduced the \"stateSelect\" enumeration in \"RelativeStates\".<br>
                                                The conversion script automatically
                                                transforms from the \"old\" to the \"new\" forms.</td></tr>
<tr><td valign=\"top\"> LossyGear<br>
                                                GearBox</td>
          <td valign=\"top\"> Renamed gear ratio parameter \"i\" to \"ratio\", in order to have a
                                                consistent naming convention.<br>
                                                Existing models are automatically converted.</td></tr>
<tr><td valign=\"top\"> SpringDamper<br>
                                                ElastoBacklash<br>
                                                Clutch<br>
                                                OneWayClutch</td>
          <td valign=\"top\"> Relative quantities (phi_rel, w_rel) are used as states, if possible
                                                (due to StateSelect.prefer). <br>
                                                In most cases, relative states in drive trains are better suited as
                                                absolute states. <br> This change might give changes in the selected states
                                                of existing models.<br>
                                                This might give rise to problems if, e.g., the initialization was not
                                                completely defined in a user model,<br> since the default
                                                initialization heuristic may give different initial values.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Moved components to structured sub-packages (Sources, Components)</td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Adaptions corresponding to Rotational</td></tr>
<tr><td valign=\"top\"> Stop</td>
          <td valign=\"top\"> Renamed to Components.MassWithStopAndFriction to be more concise.<br>
                                                MassWithStopAndFriction is not available with a support connector, <br>
                                                since the reaction force can't be modeled in a meaningful way due to reinit of velocity v.<br>
                                                Until a sound implementation of a hard stop is available, the old model may be used.</td></tr>
<tr><td colspan=\"2\"><b>Media.</b></td></tr>
<tr><td valign=\"top\"> constant nX <br>
                                                constant nXi <br>
                                                constant reference_X<br>
                                                BaseProperties</td>
          <td valign=\"top\"> The package constant nX = nS, now always, even for single species media. This also allows to define mixtures with only 1 element. The package constant nXi=if fixedX then 0 else if reducedX or nS==1 then nS - 1 else nS. This required that all BaseProperties for single species media get an additional equation to define the composition X as {1.0} (or reference_X, which is {1.0} for single species). This will also mean that all user defined single species media need to be updated by that equation.</td></tr>

<tr><td colspan=\"2\"><b>SIunits.</b></td></tr>
<tr><td valign=\"top\"> CelsiusTemperature </td>
          <td valign=\"top\"> Removed, since no SI unit. The conversion script changes references to
                                                SIunits.Conversions.NonSIunits.Temperature_degC </td></tr>
<tr><td valign=\"top\"> ThermodynamicTemperature <br>
                                                TemperatureDifference</td>
          <td valign=\"top\"> Added annotation \"absoluteValue=true/false\"
                                                in order that unit checking is possible<br>
                                                (the unit checker needs to know for a unit that has an offset,
                                                whether it is used as absolute or as a relative number)</td></tr>

<tr><td colspan=\"2\"><b>SIunits.Conversions.NonSIunits.</b></td></tr>
<tr><td valign=\"top\"> Temperature_degC<br>
                                                Temperature_degF<br>
                                                Temperature_degRk </td>
          <td valign=\"top\"> Added annotation \"absoluteValue=true\"
                                                in order that unit checking is possible<br>
                                                (the unit checker needs to know for a unit that has an offset,
                                                whether it is used as absolute or as a relative number)</td></tr>

<tr><td colspan=\"2\"><b>StateGraph.Examples.</b></td></tr>
<tr><td valign=\"top\"> ControlledTanks </td>
          <td valign=\"top\"> The connectors of the ControlledTanks did not fulfill the new
                                                restrictions of Modelica 3. This has been fixed.</td></tr>
<tr><td valign=\"top\"> Utilities </td>
          <td valign=\"top\"> Replacing inflow, outflow by connectors inflow1, inflow2,
                                                outflow1, outflow2 with appropriate input/output prefixes in
                                                order to fulfill the restrictions of Modelica 3 to arrive
                                                at balanced models. No conversion is provided, since
                                                too difficult and since the non-backward compatible change is in
                                                an example.</td></tr>

<tr><td colspan=\"2\"><b>Thermal.FluidHeatFlow.Sensors.</b></td></tr>
<tr><td valign=\"top\"> <br>
                                                pSensor<br>TSensor<br>dpSensor<br>dTSensor<br>m_flowSensor<br>V_flowSensor<br>H_flowSensor</td>
          <td valign=\"top\"> renamed to:<br>
                                                PressureSensor<br>TemperatureSensor<br>RelPressureSensor<br>RelTemperatureSensor<br>MassFlowSensor<br>VolumeFlowSensor<br>EnthalpyFlowSensor
                                                </td></tr>

<tr><td colspan=\"2\"><b>Thermal.FluidHeatFlow.Sources.</b></td></tr>
<tr><td valign=\"top\"> Ambient<br>PrescribedAmbient</td>
          <td valign=\"top\"> available as one combined component Ambient<br>
                                                Boolean parameters usePressureInput and useTemperatureInput decide
                                                whether pressure and/or temperature are constant or prescribed</td></tr>
<tr><td valign=\"top\"> ConstantVolumeFlow<br>PrescribedVolumeFlow</td>
          <td valign=\"top\"> available as one combined component VolumeFlow<br>
                                                Boolean parameter useVolumeFlowInput decides
                                                whether volume flow is constant or prescribed</td></tr>
<tr><td valign=\"top\"> ConstantPressureIncrease<br>PrescribedPressureIncrease</td>
          <td valign=\"top\"> available as one combined component PressureIncrease<br>
                                                Boolean parameter usePressureIncreaseInput decides
                                                whether pressure increase is constant or prescribed</td></tr>

<tr><td colspan=\"2\"><b>Thermal.FluidHeatFlow.Examples.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Changed the instance names of components used in the examples to more up-to-date style.</td></tr>

<tr><td colspan=\"2\"><b>Thermal.HeatTransfer.(Components)</b></td></tr>
<tr><td valign=\"top\"> HeatCapacitor</td>
          <td valign=\"top\"> Initialization changed: SteadyStateStart removed. Instead
                                                start/fixed values for T and der_T<br>(initial temperature and its derivative).</td></tr>

<tr><td valign=\"top\"> <br><br>HeatCapacitor<br>ThermalConductor<br>ThermalConvection<br>BodyRadiation<br><br>
                                                TemperatureSensor<br>RelTemperatureSensor<br>HeatFlowSensor<br><br>
                                                FixedTemperature<br>PrescribedTemperature<br>FixedHeatFlow<br>PrescribedHeatFlow</td>
          <td valign=\"top\"> Moved components to sub-packages:<br><br>
                                                Components.HeatCapacitor<br>Components.ThermalConductor<br>Components.ThermalConvection<br>Components.BodyRadiation<br><br>
                                                Sensors.TemperatureSensor<br>Sensors.RelTemperatureSensor<br>Sensors.HeatFlowSensor<br><br>
                                                Sources.FixedTemperature<br>Sources.PrescribedTemperature<br>Sources.FixedHeatFlow<br>Sources.PrescribedHeatFlow
                                                </td></tr>

<tr><td colspan=\"2\"><b>Thermal.FluidHeatFlow.Examples.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Changed the instance names of components used in the examples to more up-to-date style.</td></tr>
</table>

<p><br>
The following <b style=\"color:blue\">existing components</b>
have been <b style=\"color:blue\">improved</b> in a
<b style=\"color:blue\">backward compatible</b> way:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td valign=\"top\"> <b>Modelica.*</b> </td>
          <td valign=\"top\"> Parameter declarations, input and output function arguments without description
                                                strings improved<br> by providing meaningful description texts.
                                                </td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Continuous.</b></td></tr>
<tr><td valign=\"top\"> TransferFunction </td>
          <td valign=\"top\"> Internal scaling of the controller canonical states introduced
                                                in order to enlarge the range of transfer functions where the default
                                                relative tolerance of the simulator is sufficient.</td>
</tr>

<tr><td valign=\"top\"> Butterworth<br>CriticalDamping </td>
          <td valign=\"top\"> Documentation improved and plots of the filter characteristics added.</td></tr>

<tr><td colspan=\"2\"><b>Electrical.Analog.Basic.</b></td></tr>
<tr><td valign=\"top\"> EMF </td>
          <td valign=\"top\"> New parameter \"useSupport\" to optionally enable a support connector.</td></tr>

<tr><td colspan=\"2\"><b>Icons.</b></td></tr>
<tr><td valign=\"top\"> TranslationalSensor<br>
                                                RotationalSensor</td>
          <td valign=\"top\"> Removed drawing from the diagram layer (kept drawing only in
                                                icon layer),<br> in order that this icon can be used in situations
                                                where components are dragged in the diagram layer.</td></tr>

<tr><td colspan=\"2\"><b>Math.Vectors.</b></td></tr>
<tr><td valign=\"top\"> normalize</td>
          <td valign=\"top\"> Implementation changed, so that the result is awalys continuous<br>
                                                (previously, this was not the case for small vectors: normalize(eps,eps)).
                                                </td></tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\"> </td>
          <td valign=\"top\"> Renamed non-standard keywords defineBranch, defineRoot, definePotentialRoot,
                                                isRooted to the standard names:<br>
                                                Connections.branch/.root/.potentialRoot/.isRooted.</td></tr>
<tr><td valign=\"top\"> Frames </td>
          <td valign=\"top\"> Added annotation \"Inline=true\" to all one-line functions
                                                (which should be all inlined).</td></tr>
<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Parts.</b></td></tr>
<tr><td valign=\"top\"> Mounting1D<br>
                                                Rotor1D<br>
                                                BevelGear1D</td>
          <td valign=\"top\"> Changed implementation so that no longer modifiers for connector
                                                variables are used,<br>because this violates the restrictions on
                                                \"balanced models\" of Modelica 3.</td></tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> InitializeFlange</td>
          <td valign=\"top\"> Changed implementation so that counting unknowns and
                                                equations is possible without actual values of parameters.</td></tr>

<tr><td colspan=\"2\"><b>Thermal.FluidHeatFlow.Interfaces.Partials.</b></td></tr>
<tr><td valign=\"top\">TwoPort</td>
          <td valign=\"top\"> Introduced <code>parameter Real tapT(final min=0, final max=1)=1</code> <br> that defines the temperature of the heatPort
                                                between inlet and outlet.</td></tr>

<tr><td colspan=\"2\"><b>StateGraph.</b></td></tr>
<tr><td valign=\"top\"> InitialStep<br>
                                                InitialStepWithSignal<br>
                                                Step<br>
                                                StepWithSignal</td>
          <td valign=\"top\"> Changed implementation so that no longer modifiers for output
                                                variables are used,<br>because this violates the restrictions on
                                                \"balanced models\" of Modelica 3.</td></tr>

</table>

<p><br>
The following <b style=\"color:red\">critical errors</b> have been fixed (i.e., errors
that can lead to wrong simulation results):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Electrical.Analog.Examples.</b></td></tr>
<tr><td valign=\"top\"> CauerLowPassSC </td>
          <td valign=\"top\"> Wrong calculation of Capacitor1 both in Rn and Rp corrected
                                                (C=clock/R instead of C=clock*R) </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Parts.</b></td></tr>
<tr><td valign=\"top\"> Rotor1D </td>
          <td valign=\"top\"> The 3D reaction torque was not completely correct and gave in
                                                some situations a wrong result. This bug should not influence the
                                                movement of a multi-body system, but only the constraint torques
                                                are sometimes not correct.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> ElastoBacklash </td>
          <td valign=\"top\"> If the damping torque was too large, the reaction torque
                                                could \"pull\" which is unphysical. The component was
                                                newly written by limiting the damping torque in such a case
                                                so that \"pulling\" torques can no longer occur. Furthermore,
                                                during initialization the characteristics is made continuous
                                                to reduce numerical errors. The relative angle and relative
                                                angular velocities are used as states, if possible
                                                (StateSelect.prefer), since relative quantities lead usually
                                                to better behavior.  </td>
</tr>
<tr><td valign=\"top\"> Position<br>Speed<br>Accelerate<br>Move</td>
          <td valign=\"top\"> The movement of the flange was wrongly defined as absolute;
                                                this is corrected as relative to connector support.<br>
                                                For Accelerate, it was necessary to rename
                                                RealInput a to a_ref, as well as the start values
                                                phi_start to phi.start and w_start to w.start.
                                                The conversion script performs the necessary conversion of
                                                existing models automatically.</td>
</tr>
<tr><td colspan=\"2\"><b>Media.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialSimpleIdealGasMedium </td>
          <td valign=\"top\"> Inconsistency in reference temperature corrected. This may give
                                                different results for functions:<br>
                                                specificEnthalpy, specificInternalEnergy, specificGibbsEnergy,
                                                specificHelmholtzEnergy.</td>
</tr>
<tr><td colspan=\"2\"><b>Media.Air.</b></td></tr>
<tr><td valign=\"top\"> specificEntropy </td>
          <td valign=\"top\"> Small bug in entropy computation of ideal gas mixtures corrected.</td>
</tr>
<tr><td colspan=\"2\"><b>Media.IdealGases.Common.MixtureGasNasa</b></td></tr>
<tr><td valign=\"top\"> specificEntropy </td>
          <td valign=\"top\"> Small bug in entropy computation of ideal gas mixtures corrected.</td>
</tr>
</table>

<p><br>
The following <b style=\"color:red\">uncritical errors</b> have been fixed (i.e., errors
that do <b style=\"color:red\">not</b> lead to wrong simulation results, but, e.g.,
units are wrong or errors in documentation):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.Tables.</b></td></tr>
<tr><td valign=\"top\"> CombiTable2D</td>
          <td valign=\"top\"> Documentation improved.</td>
</tr>

<tr><td colspan=\"2\"><b>Electrica.Digital.Gates</b></td></tr>
<tr><td valign=\"top\"> AndGate<br>
                                                NandGate<br>
                                                OrGate<br>
                                                NorGate<br>
                                                XorGate<br>
                                                XnorGate</td>
          <td valign=\"top\"> The number of inputs was not correctly propagated
                                                to the included base model.<br>
                                                This gave a translation error, if the number
                                                of inputs was changed (and not the default used).</td>
</tr>

<tr><td colspan=\"2\"><b>Electrica.Digital.Sources</b></td></tr>
<tr><td valign=\"top\"> Pulse </td>
          <td valign=\"top\"> Model differently implemented, so that
                                                warning message about \"cannot properly initialize\" is gone.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> BearingFriction<br>
                                                Clutch<br>
                                                OneWayClutch<br>
                                                Brake <br>
                                                Gear </td>
          <td valign=\"top\"> Declaration of table parameter changed from
                                                table[:,:] to table[:,2].</td>
</tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Examples.Loops.Utilities.</b></td></tr>
<tr><td valign=\"top\"> GasForce </td>
          <td valign=\"top\"> Unit of variable \"press\" corrected (from Pa to bar)</td>
</tr>

<tr><td colspan=\"2\"><b>StateGraph.Examples.</b></td></tr>
<tr><td valign=\"top\">SimpleFriction</td>
          <td valign=\"top\"> The internal parameter k is defined and calculated with the appropriate unit.</td></tr>

<tr><td colspan=\"2\"><b>Thermal.FluidHeatFlow.Interfaces.Partials.</b></td></tr>
<tr><td valign=\"top\">SimpleFriction</td>
          <td valign=\"top\"> The internal parameter k is defined and calculated with the appropriate unit.</td></tr>

</table>

</html>"));
end Version_3_0;

class Version_2_2_2 "Version 2.2.2 (Aug. 31, 2007)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>
<p>
Version 2.2.2 is backward compatible to version 2.2.1 and 2.2 with
the following exceptions:
</p>
<ul>
<li> Removal of package Modelica.Media.Interfaces.PartialTwoPhaseMediumWithCache
         (this was not yet utilized).</li>
<li> Removal of the media packages in
         Modelica.Media.IdealGases.SingleGases that are not type compatible
         to Modelica.Media.Interfaces.PartialMedium, because a FluidConstants
         record definition is missing,
         for details, see
          <a href=\"modelica://Modelica.Media.IdealGases\">Modelica.Media.IdealGases</a>
         (this is seen as a bug fix).</li>
</ul>

<p>
An overview of the differences between version 2.2.2 and the previous
version 2.2.1 is given below. The exact differences (but without
differences in the documentation) are available in
<a href=\"modelica://Modelica/Resources/Documentation/Differences-Modelica-221-222.html\">Differences-Modelica-221-222.html</a>.
This comparison file was generated automatically with Dymola's
ModelManagement.compare function.
</p>

<p>
In this version, <b>no</b> new libraries have been added. The <b>documentation</b>
of the whole library was improved.
</p>

<p><br>
The following <b style=\"color:blue\">new components</b> have been added
to <b style=\"color:blue\">existing</b> libraries:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.Logical.</b></td></tr>
<tr><td valign=\"top\"> TerminateSimulation</td>
          <td valign=\"top\"> Terminate a simulation by a given condition.</td>
</tr>

<tr><td colspan=\"2\"><b>Blocks.Routing.</b></td></tr>
<tr><td valign=\"top\"> RealPassThrough<br>
                   IntegerPassThrough<br>
                   BooleanPassThrough</td>
          <td valign=\"top\"> Pass a signal from input to output
                   (useful in combination with a bus due to restrictions
                   of expandable connectors).</td>
</tr>

<tr><td colspan=\"2\"><b>Blocks.Sources.</b></td></tr>
<tr><td valign=\"top\"> KinematicPTP2 </td>
          <td valign=\"top\"> Directly gives q,qd,qdd as output (and not just qdd as KinematicPTP).
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Examples.</b></td></tr>
<tr><td valign=\"top\"> TransformerTestbench </td>
          <td valign=\"top\"> Transformer Testbench
          </td></tr>
<tr><td valign=\"top\"> Rectifier6pulse </td>
          <td valign=\"top\"> 6-pulse rectifier with 1 transformer
          </td>
</tr>
<tr><td valign=\"top\"> Rectifier12pulse </td>
          <td valign=\"top\"> 12-pulse rectifier with 2 transformers
          </td>
</tr>
<tr><td valign=\"top\"> AIMC_Steinmetz </td>
          <td valign=\"top\"> Asynchronous induction machine squirrel cage with Steinmetz connection
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.BasicMachines.Components.</b></td></tr>
<tr><td valign=\"top\"> BasicAIM </td>
          <td valign=\"top\"> Partial model for asynchronous induction machine
          </td></tr>
<tr><td valign=\"top\"> BasicSM </td>
          <td valign=\"top\"> Partial model for synchronous induction machine
          </td></tr>
<tr><td valign=\"top\"> PartialAirGap </td>
          <td valign=\"top\"> Partial airgap model
          </td></tr>
<tr><td valign=\"top\"> BasicDCMachine </td>
          <td valign=\"top\"> Partial model for DC machine
          </td></tr>
<tr><td valign=\"top\"> PartialAirGapDC </td>
          <td valign=\"top\"> Partial airgap model of a DC machine
          </td></tr>
<tr><td valign=\"top\"> BasicTransformer </td>
          <td valign=\"top\"> Partial model of threephase transformer
          </td></tr>
<tr><td valign=\"top\"> PartialCore </td>
          <td valign=\"top\"> Partial model of transformer core with 3 windings
          </td></tr>
<tr><td valign=\"top\"> IdealCore </td>
          <td valign=\"top\"> Ideal transformer with 3 windings
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.BasicMachines.</b></td></tr>
<tr><td valign=\"top\"> Transformers </td>
          <td valign=\"top\"> Sub-Library for technical 3phase transformers
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> Adapter </td>
          <td valign=\"top\"> Adapter to model housing of electrical machine
          </td>
</tr>
<tr><td colspan=\"2\"><b>Math.</b></td></tr>
<tr><td valign=\"top\"> Vectors </td>
          <td valign=\"top\"> New library of functions operating on vectors
          </td>
</tr>
<tr><td valign=\"top\"> atan3 </td>
          <td valign=\"top\"> Four quadrant inverse tangent (select solution that is closest to given angle y0)
          </td>
</tr>
<tr><td valign=\"top\"> asinh </td>
          <td valign=\"top\"> Inverse of sinh (area hyperbolic sine)
          </td>
</tr>
<tr><td valign=\"top\"> acosh </td>
          <td valign=\"top\"> Inverse of cosh (area hyperbolic cosine)
          </td>
</tr>

<tr><td colspan=\"2\"><b>Math.Vectors</b></td></tr>
<tr><td valign=\"top\"> isEqual </td>
          <td valign=\"top\"> Determine if two Real vectors are numerically identical
          </td>
</tr>
<tr><td valign=\"top\"> norm </td>
          <td valign=\"top\"> Return the p-norm of a vector
          </td></tr>
<tr><td valign=\"top\"> length </td>
          <td valign=\"top\"> Return length of a vector (better as norm(), if further symbolic processing is performed)
          </td></tr>
<tr><td valign=\"top\"> normalize </td>
          <td valign=\"top\"> Return normalized vector such that length = 1 and prevent zero-division for zero vector
          </td></tr>
<tr><td valign=\"top\"> reverse </td>
          <td valign=\"top\"> Reverse vector elements (e.g., v[1] becomes last element)
          </td></tr>
<tr><td valign=\"top\"> sort </td>
          <td valign=\"top\"> Sort elements of vector in ascending or descending order
          </td></tr>

<tr><td colspan=\"2\"><b>Math.Matrices</b></td></tr>
<tr><td valign=\"top\"> solve2 </td>
          <td valign=\"top\"> Solve real system of linear equations A*X=B with a B matrix
                   (Gaussian elimination with partial pivoting)
          </td>
</tr>
<tr><td valign=\"top\"> LU_solve2 </td>
          <td valign=\"top\"> Solve real system of linear equations P*L*U*X=B with a B matrix
                   and an LU decomposition (from LU(..))
          </td></tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> InitializeFlange </td>
          <td valign=\"top\"> Initialize a flange according to given signals
                   (useful if initialization signals are provided by a signal bus).
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialMedium.</b></td></tr>
<tr><td valign=\"top\"> density_pTX </td>
          <td valign=\"top\"> Return density from p, T, and X or Xi
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialTwoPhaseMedium.</b></td></tr>
<tr><td valign=\"top\"> BaseProperties </td>
          <td valign=\"top\"> Base properties (p, d, T, h, u, R, MM, x) of a two phase medium
          </td>
</tr>
<tr><td valign=\"top\"> molarMass </td>
          <td valign=\"top\"> Return the molar mass of the medium
          </td>
</tr>
<tr><td valign=\"top\"> saturationPressure_sat </td>
          <td valign=\"top\"> Return saturation pressure
          </td>
</tr>
<tr><td valign=\"top\"> saturationTemperature_sat </td>
          <td valign=\"top\"> Return saturation temperature
          </td>
</tr>
<tr><td valign=\"top\"> saturationTemperature_derp_sat </td>
          <td valign=\"top\"> Return derivative of saturation temperature w.r.t. pressure
          </td>
</tr>  <tr><td valign=\"top\"> setState_px </td>
          <td valign=\"top\"> Return thermodynamic state from pressure and vapour quality
          </td>
</tr>  <tr><td valign=\"top\"> setState_Tx </td>
          <td valign=\"top\"> Return thermodynamic state from temperature and vapour quality
          </td>
</tr>  <tr><td valign=\"top\"> vapourQuality </td>
          <td valign=\"top\"> Return vapour quality
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialLinearFluid </td>
          <td valign=\"top\"> Generic pure liquid model with constant cp,
                   compressibility and thermal expansion coefficients
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Air.MoistAir.</b></td></tr>
<tr><td valign=\"top\"> massFraction_pTphi </td>
          <td valign=\"top\"> Return the steam mass fraction from relative humidity and T
          </td>
</tr>
<tr><td valign=\"top\"> saturationTemperature </td>
          <td valign=\"top\"> Return saturation temperature from (partial) pressure
                   via numerical inversion of function saturationPressure
          </td>
</tr>
<tr><td valign=\"top\"> enthalpyOfWater </td>
          <td valign=\"top\"> Return specific enthalpy of water (solid/liquid) near
                   atmospheric pressure from temperature
          </td>
</tr>
<tr><td valign=\"top\"> enthalpyOfWater_der </td>
          <td valign=\"top\"> Return derivative of enthalpyOfWater()\" function
          </td>
</tr>
<tr><td valign=\"top\"> PsychrometricData </td>
          <td valign=\"top\"> Model to generate plot data for psychrometric chart
          </td>
</tr>
<tr><td colspan=\"2\"><b>Media.CompressibleLiquids.</b><br>
          New sub-library for simple compressible liquid models</td></tr>
<tr><td valign=\"top\"> LinearColdWater </td>
          <td valign=\"top\"> Cold water model with linear compressibility
          </td>
</tr>
<tr><td valign=\"top\"> LinearWater_pT_Ambient </td>
          <td valign=\"top\"> Liquid, linear compressibility water model at 1.01325 bar
                   and 25 degree Celsius
          </td>
</tr>
<tr><td colspan=\"2\"><b>SIunits.</b></td></tr>
<tr><td valign=\"top\"> TemperatureDifference </td>
          <td valign=\"top\"> Type for temperature difference
          </td>
</tr>
</table>

<p><br>
The following <b style=\"color:blue\">existing components</b>
have been <b style=\"color:blue\">improved</b>:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.Examples.</b></td></tr>
<tr><td valign=\"top\"> BusUsage</td>
          <td valign=\"top\"> Example changed from the \"old\" to the \"new\" bus concept with
                   expandable connectors.</td></tr>

<tr><td colspan=\"2\"><b>Blocks.Discrete.</b></td></tr>
<tr><td valign=\"top\"> ZeroOrderHold</td>
          <td valign=\"top\"> Sample output ySample moved from \"protected\" to \"public\"
                   section with new attributes (start=0, fixed=true).
          </td>
</tr>
<tr><td valign=\"top\"> TransferFunction</td>
          <td valign=\"top\"> Discrete state x with new attributes (each start=0, each fixed=0).
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.</b></td></tr>
<tr><td valign=\"top\"> Analog<br>MultiPhase</td>
          <td valign=\"top\"> Improved some icons.
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> MISO</td>
          <td valign=\"top\"> Removed \"algorithm\" from this partial block.
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital.Delay.</b></td></tr>
<tr><td valign=\"top\"> DelayParams</td>
          <td valign=\"top\"> Removed \"algorithm\" from this partial block.
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital.Delay.</b></td></tr>
<tr><td valign=\"top\"> DelayParams</td>
          <td valign=\"top\"> Removed \"algorithm\" from this partial block.
          </td>
</tr>
<tr><td valign=\"top\"> TransportDelay</td>
          <td valign=\"top\">  If delay time is zero, an infinitely small delay is
                        introduced via pre(x) (previously \"x\" was used).
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital.Sources.</b></td></tr>
<tr><td valign=\"top\"> Clock<br>Step</td>
          <td valign=\"top\"> Changed if-conditions from \"xxx < time\" to \"time >= xxx\"
                   (according to the Modelica specification, in the second case
                   a time event should be triggered, i.e., this change leads
                   potentially to a faster simulation).
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital.Converters.</b></td></tr>
<tr><td valign=\"top\"> BooleanToLogic<br>
                   LogicToBoolean<br>
                   RealToLogic<br>
                   LogicToReal</td>
          <td valign=\"top\"> Changed from \"algorithm\" to \"equation\" section
                   to allow better symbolic preprocessing
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.</b></td></tr>
<tr><td valign=\"top\"> Machines</td>
          <td valign=\"top\"> Slightly improved documentation, typos in
                   documentation corrected
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Examples.</b></td></tr>
<tr><td valign=\"top\"> AIMS_start</td>
          <td valign=\"top\"> Changed QuadraticLoadTorque1(TorqueDirection=true) to
                   QuadraticLoadTorque1(TorqueDirection=false) since more realistic
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialBasicMachine</td>
          <td valign=\"top\"> Introduced support flange to model the
                   reaction torque to the housing
          </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Machines.Sensors.</b></td></tr>
<tr><td valign=\"top\"> Rotorangle</td>
          <td valign=\"top\"> Introduced support flange to model the
                   reaction torque to the housing
          </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Examples.Elementary.</b></td></tr>
<tr><td valign=\"top\"> PointMassesWithGravity</td>
          <td valign=\"top\"> Added two point masses connected by a line force to demonstrate
                   additionally how this works. Connections of point masses
                   with 3D-elements are demonstrated in the new example
                   PointMassesWithGravity (there is the difficulty that the orientation
                   is not defined in a PointMass object and therefore some
                   special handling is needed in case of a connection with
                   3D-elements, where the orientation of the point mass is not
                   determined by these elements.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Examples.Systems.</b></td></tr>
<tr><td valign=\"top\"> RobotR3</td>
          <td valign=\"top\"> Changed from the \"old\" to the \"new\" bus concept with expandable connectors.
                   Replaced the non-standard Modelica function \"constrain()\" by
                   standard Modelica components. As a result, the non-standard function
                   constrain() is no longer used in the Modelica Standard Library.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Frames.Orientation.</b></td></tr>
<tr><td valign=\"top\"> equalityConstraint</td>
          <td valign=\"top\"> Use a better residual for the equalityConstraint function.
                   As a result, the non-linear equation system of a kinematic
                   loop is formulated in a better way (the range where the
                   desired result is a unique solution of the non-linear
                   system of equations becomes much larger).</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\"> Visualizers.</td>
          <td valign=\"top\"> Removed (misleading) annotation \"structurallyIncomplete\"
                   in the models of this sub-library
          </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> Examples</td>
          <td valign=\"top\"> For all models in this sub-library:
                   <ul>
                   <li> Included a housing object in all examples to compute
                                all support torques.</li>
                   <li> Replaced initialization by modifiers via the
                                initialization menu parameters of Inertia components.</li>
                   <li> Removed \"encapsulated\" and unnecessary \"import\".</li>
                   <li> Included \"StopTime\" in the annotations.</li>
                   </ul>
          </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> FrictionBase</td>
          <td valign=\"top\"> Introduced \"fixed=true\" for Boolean variables startForward,
                   startBackward, mode.
          </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> FrictionBase</td>
          <td valign=\"top\"> Introduced \"fixed=true\" for Boolean variables startForward,
                   startBackward, mode.
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.UsersGuide.MediumUsage.</b></td></tr>
<tr><td valign=\"top\"> TwoPhase</td>
          <td valign=\"top\"> Improved documentation and demonstrating the newly introduced functions
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Examples.</b></td></tr>
<tr><td valign=\"top\"> WaterIF97</td>
          <td valign=\"top\"> Provided (missing) units for variables V, dV, H_flow_ext, m, U.
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialMedium</td>
          <td valign=\"top\"> Final modifiers are removed from nX and nXi, to allow
                   customized medium models such as mixtures of refrigerants with oil, etc.
          </td>
</tr>
<tr><td valign=\"top\"> PartialCondensingGases</td>
          <td valign=\"top\"> Included attributes \"min=1, max=2\" for input argument FixedPhase
                   for functions setDewState and setBubbleState (in order to guarantee
                   that input arguments are correct).
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Interfaces.PartialMedium.</b></td></tr>
<tr><td valign=\"top\"> BaseProperties</td>
          <td valign=\"top\"> New Boolean parameter \"standardOrderComponents\".
                   If true, last element vector X is computed from 1-sum(Xi) (= default)
                   otherwise, no equation is provided for it in PartialMedium.
          </td>
</tr>
<tr><td valign=\"top\"> IsentropicExponent</td>
          <td valign=\"top\"> \"max\" value changed from 1.7 to 500000
          </td>
</tr>
<tr><td valign=\"top\"> setState_pTX<br>
                   setState_phX<br>
                   setState_psX<br>
                   setState_dTX<br>
                   specificEnthalpy_pTX<br>
                   temperature_phX<br>
                   density_phX<br>
                   temperature_psX<br>
                   density_psX<br>
                   specificEnthalpy_psX</td>
          <td valign=\"top\"> Introduced default value \"reference_X\" for input argument \"X\".
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Interfaces.PartialSimpleMedium.</b></td></tr>
<tr><td valign=\"top\"> setState_pTX<br>
                   setState_phX<br>
                   setState_psX<br>
                   setState_dTX</td>
          <td valign=\"top\"> Introduced default value \"reference_X\" for input argument \"X\".
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Interfaces.PartialSimpleIdealGasMedium.</b></td></tr>
<tr><td valign=\"top\"> setState_pTX<br>
                   setState_phX<br>
                   setState_psX<br>
                   setState_dTX</td>
          <td valign=\"top\"> Introduced default value \"reference_X\" for input argument \"X\".
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Air.MoistAir.</b></td></tr>
<tr><td valign=\"top\"> setState_pTX<br>
                   setState_phX<br>
                   setState_dTX</td>
          <td valign=\"top\"> Introduced default value \"reference_X\" for input argument \"X\".
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.IdealGases.Common.SingleGasNasa.</b></td></tr>
<tr><td valign=\"top\"> setState_pTX<br>
                   setState_phX<br>
                   setState_psX<br>
                   setState_dTX</td>
          <td valign=\"top\"> Introduced default value \"reference_X\" for input argument \"X\".
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.IdealGases.Common.MixtureGasNasa.</b></td></tr>
<tr><td valign=\"top\"> setState_pTX<br>
                   setState_phX<br>
                   setState_psX<br>
                   setState_dTX<br>
                   h_TX</td>
          <td valign=\"top\"> Introduced default value \"reference_X\" for input argument \"X\".
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Common.</b></td></tr>
<tr><td valign=\"top\"> IF97PhaseBoundaryProperties<br>
                   gibbsToBridgmansTables </td>
          <td valign=\"top\"> Introduced unit for variables vt, vp.
          </td>
</tr>
<tr><td valign=\"top\"> SaturationProperties</td>
          <td valign=\"top\"> Introduced unit for variable dpT.
          </td>
</tr>
<tr><td valign=\"top\"> BridgmansTables</td>
          <td valign=\"top\"> Introduced unit for dfs, dgs.
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Common.ThermoFluidSpecial.</b></td></tr>
<tr><td valign=\"top\"> gibbsToProps_ph<br>
                   gibbsToProps_ph  <br>
                   gibbsToBoundaryProps<br>
                   gibbsToProps_dT<br>
                   gibbsToProps_pT</td>
          <td valign=\"top\"> Introduced unit for variables vt, vp.
          </td></tr>
<tr><td valign=\"top\"> TwoPhaseToProps_ph</td>
          <td valign=\"top\"> Introduced unit for variables dht, dhd, detph.
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.</b></td></tr>
<tr><td valign=\"top\"> MoistAir</td>
          <td valign=\"top\"> Documentation of moist air model significantly improved.
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.MoistAir.</b></td></tr>
<tr><td valign=\"top\"> enthalpyOfVaporization</td>
          <td valign=\"top\"> Replaced by linear correlation since simpler and more
                   accurate in the entire region.
          </td>
</tr>

<tr><td colspan=\"2\"><b> Media.Water.IF97_Utilities.BaseIF97.Regions.</b></td></tr>
<tr><td valign=\"top\"> drhovl_dp</td>
          <td valign=\"top\"> Introduced unit for variable dd_dp.
          </td>
</tr>

<tr><td colspan=\"2\"><b> Thermal.</b></td></tr>
<tr><td valign=\"top\"> FluidHeatFlow</td>
          <td valign=\"top\"> Introduced new parameter tapT (0..1) to define the
                   temperature of the HeatPort as linear combination of the
                   flowPort_a (tapT=0) and flowPort_b (tapT=1) temperatures.
          </td>
</tr>
</table>

<p><br>
The following <b style=\"color:red\">critical errors</b> have been fixed (i.e., errors
that can lead to wrong simulation results):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Electrical.Machines.BasicMachines.Components.</b></td></tr>
<tr><td valign=\"top\"> ElectricalExcitation</td>
          <td valign=\"top\"> Excitation voltage ve is calculated as
                   \"spacePhasor_r.v_[1]*TurnsRatio*3/2\" instead of
                   \"spacePhasor_r.v_[1]*TurnsRatio
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Parts.</b></td></tr>
<tr><td valign=\"top\"> FixedRotation</td>
          <td valign=\"top\"> Bug corrected that the torque balance was wrong in the
                   following cases (since vector r was not transformed
                   from frame_a to frame_b; note this special case occurs very seldom in practice):
                   <ul><li> frame_b is in the spanning tree closer to the root
                                        (usually this is frame_a).</li>
                           <li> vector r from frame_a to frame_b is not zero.</li>
                   </ul>
           </td>
</tr>

<tr><td valign=\"top\"> PointMass</td>
         <td valign=\"top\"> If a PointMass model is connected so that no equations are present
                  to compute its orientation object, the orientation was arbitrarily
                  set to a unit rotation. In some cases this can lead to a wrong overall
                  model, depending on how the PointMass model is used. For this reason,
                  such cases lead now to an error (via an assert(..)) with an explanation
                  how to fix this.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialPureSubstance.</b></td></tr>
<tr><td valign=\"top\"> pressure_dT<br>
                   specificEnthalpy_dT
          </td>
          <td valign=\"top\"> Changed wrong call from \"setState_pTX\" to \"setState_dTX\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialTwoPhaseMedium.</b></td></tr>
<tr><td valign=\"top\"> pressure_dT<br>
                   specificEnthalpy_dT
          </td>
          <td valign=\"top\"> Changed wrong call from \"setState_pTX\" to \"setState_dTX\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Common.ThermoFluidSpecial.</b></td></tr>
<tr><td valign=\"top\"> gibbsToProps_dT<br>
                   helmholtzToProps_ph<br>
                   helmholtzToProps_pT<br>
                   helmholtzToProps_dT</td>
          <td valign=\"top\"> Bugs in equations corrected </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Common.</b></td></tr>
<tr><td valign=\"top\"> helmholtzToBridgmansTables<br>
                   helmholtzToExtraDerivs</td>
          <td valign=\"top\"> Bugs in equations corrected </td>
</tr>

<tr><td colspan=\"2\"><b>Media.IdealGases.Common.SingleGasNasa.</b></td></tr>
<tr><td valign=\"top\"> density_derp_T</td>
          <td valign=\"top\"> Bug in equation of partial derivative corrected </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.IF97_Utilities.</b></td></tr>
<tr><td valign=\"top\"> BaseIF97.Inverses.dtofps3<br>
                   isentropicExponent_props_ph<br>
                   isentropicExponent_props_pT<br>
                   isentropicExponent_props_dT</td>
          <td valign=\"top\"> Bugs in equations corrected </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Air.MoistAir.</b></td></tr>
<tr><td valign=\"top\"> h_pTX</td>
          <td valign=\"top\"> Bug in setState_phX due to wrong vector size in h_pTX corrected.
                   Furthermore, syntactical errors corrected:
                   <ul><li> In function massFractionpTphi an equation
                                        sign is used in an algorithm.</li>
                           <li> Two consecutive semicolons removed</li>
                   </ul>
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.</b></td></tr>
<tr><td valign=\"top\"> waterConstants</td>
          <td valign=\"top\"> Bug in equation of criticalMolarVolume corrected.
          </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.IF97_Utilities.BaseIF97.Regions.</b></td></tr>
<tr><td valign=\"top\"> region_ph<br>
                   region_ps</td>
          <td valign=\"top\"> Bug in region determination corrected.
          </td>
</tr>

<tr><td valign=\"top\"> boilingcurve_p<br>
                   dewcurve_p</td>
          <td valign=\"top\"> Bug in equation of plim corrected.
          </td>
</tr>
</table>

<p><br>
The following <b style=\"color:red\">uncritical errors</b> have been fixed (i.e., errors
that do <b style=\"color:red\">not</b> lead to wrong simulation results, but, e.g.,
units are wrong or errors in documentation):
</p>

<table border=\"1\" cellspacing=0 cellpadding=2 style=\"border-collapse:collapse;\">
<tr><td colspan=\"2\"><b>Blocks.</b></td></tr>
<tr><td valign=\"top\"> Examples</td>
          <td valign=\"top\"> Corrected typos in description texts of bus example models.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Blocks.Continuous.</b></td></tr>
<tr><td valign=\"top\"> LimIntegrator</td>
          <td valign=\"top\"> removed incorrect smooth(0,..) because expression might be discontinuous.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Blocks.Math.UnitConversions.</b></td></tr>
<tr><td valign=\"top\"> block_To_kWh<br>block_From_kWh</td>
          <td valign=\"top\"> Corrected unit from \"kWh\" to (syntactically correct) \"kW.h\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Analog.Examples.</b></td></tr>
<tr><td valign=\"top\"> HeatingNPN_OrGate</td>
          <td valign=\"top\"> Included start values, so that initialization is
                                                successful </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Analog.Lines.</b></td></tr>
<tr><td valign=\"top\"> OLine</td>
          <td valign=\"top\"> Corrected unit from \"Siemens/m\" to \"S/m\".
           </td></tr>
<tr><td valign=\"top\"> TLine2</td>
          <td valign=\"top\"> Changed wrong type of parameter NL (normalized length) from
                   SIunits.Length to Real.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital.Delay.</b></td></tr>
<tr><td valign=\"top\"> TransportDelay</td>
          <td valign=\"top\"> Syntax error corrected
                   (\":=\" in equation section is converted by Dymola silently to \"=\").
           </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.Digital</b></td></tr>
<tr><td valign=\"top\"> Converters</td>
          <td valign=\"top\"> Syntax error corrected
                   (\":=\" in equation section is converted by Dymola silently to \"=\").
           </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.MultiPhase.Basic.</b></td></tr>
<tr><td valign=\"top\"> Conductor</td>
          <td valign=\"top\"> Changed wrong type of parameter G from SIunits.Resistance to
                   SIunits.Conductance.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.MultiPhase.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> Plug<br></td>
          <td valign=\"top\"> Made used \"pin\" connectors non-graphical (otherwise,
                   there are difficulties to connect to Plug).
           </td>
</tr>

<tr><td colspan=\"2\"><b>Electrical.MultiPhase.Sources.</b></td></tr>
<tr><td valign=\"top\"> SineCurrent</td>
          <td valign=\"top\"> Changed wrong type of parameter offset from SIunits.Voltage to
                   SIunits.Current.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Examples.Loops.</b></td></tr>
<tr><td valign=\"top\"> EngineV6</td>
          <td valign=\"top\"> Corrected wrong crankAngleOffset of some cylinders
                   and improved the example.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Examples.Loops.Utilities.</b></td></tr>
<tr><td valign=\"top\"> GasForce</td>
          <td valign=\"top\"> Wrong units corrected:
                   \"SIunitsPosition x,y\" to \"Real x,y\";
           \"SIunits.Pressure press\" to \"SIunits.Conversions.NonSIunits.Pressure_bar\"
           </td>
</tr>
<tr><td valign=\"top\"> GasForce2</td>
          <td valign=\"top\"> Wrong unit corrected: \"SIunits.Position x\" to \"Real x\".
           </td>
</tr>
<tr><td valign=\"top\"> EngineV6_analytic</td>
          <td valign=\"top\"> Corrected wrong crankAngleOffset of some cylinders.
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> PartialLineForce</td>
          <td valign=\"top\"> Corrected wrong unit: \"SIunits.Position eRod_a\" to \"Real eRod_a\";
           </td>
</tr>
<tr><td valign=\"top\"> FlangeWithBearingAdaptor </td>
          <td valign=\"top\"> If includeBearingConnector = false, connector \"fr\"
                           + \"ame\" was not
                   removed. As long as the connecting element to \"frame\" determines
                   the non-flow variables, this is fine. In the corrected version, \"frame\"
                   is conditionally removed in this case.</td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Forces.</b></td></tr>
<tr><td valign=\"top\"> ForceAndTorque</td>
          <td valign=\"top\"> Corrected wrong unit: \"SIunits.Force t_b_0\" to \"SIunits.Torque t_b_0\".
           </td>
</tr>
<tr><td valign=\"top\"> LineForceWithTwoMasses</td>
          <td valign=\"top\"> Corrected wrong unit: \"SIunits.Position e_rel_0\" to \"Real e_rel_0\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Frames.</b></td></tr>
<tr><td valign=\"top\"> axisRotation</td>
          <td valign=\"top\"> Corrected wrong unit: \"SIunits.Angle der_angle\" to
                        \"SIunits.AngularVelocity der_angle\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Joints.Assemblies.</b></td></tr>
<tr><td valign=\"top\"> JointUSP<br>JointSSP</td>
          <td valign=\"top\"> Corrected wrong unit: \"SIunits.Position aux\"  to \"Real\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Sensors.</b></td></tr>
<tr><td valign=\"top\"> AbsoluteSensor</td>
          <td valign=\"top\"> Corrected wrong units: \"SIunits.Acceleration angles\" to
                   \"SIunits.Angle angles\" and
                   \"SIunits.Velocity w_abs_0\" to \"SIunits.AngularVelocity w_abs_0\"
           </td>
</tr>
<tr><td valign=\"top\"> RelativeSensor</td>
          <td valign=\"top\"> Corrected wrong units: \"SIunits.Acceleration angles\" to
                   \"SIunits.Angle angles\"
           </td>
</tr>
<tr><td valign=\"top\"> Distance</td>
          <td valign=\"top\"> Corrected wrong units: \"SIunits.Length L2\" to \"SIunits.Area L2\" and
                   SIunits.Length s_small2\" to \"SIunits.Area s_small2\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.MultiBody.Visualizers.Advanced.</b></td></tr>
<tr><td valign=\"top\"> Shape</td>
          <td valign=\"top\"> Changed \"MultiBody.Types.Color color\" to \"Real color[3]\", since
                   \"Types.Color\" is \"Integer color[3]\" and there have been backward
                   compatibility problems with models using \"color\" before it was changed
                   to \"Types.Color\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> FrictionBase</td>
          <td valign=\"top\"> Rewrote equations with new variables \"unitAngularAcceleration\" and
                   \"unitTorque\" in order that the equations are correct with respect
                   to units (previously, variable \"s\" can be both a torque and an
                   angular acceleration and this lead to unit incompatibilities)
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> OneWayClutch<br>LossyGear</td>
          <td valign=\"top\"> Rewrote equations with new variables \"unitAngularAcceleration\" and
                   \"unitTorque\" in order that the equations are correct with respect
                   to units (previously, variable \"s\" can be both a torque and an
                   angular acceleration and this lead to unit incompatibilities)
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> FrictionBase</td>
          <td valign=\"top\"> Rewrote equations with new variables \"unitAngularAcceleration\" and
                   \"unitTorque\" in order that the equations are correct with respect
                   to units (previously, variable \"s\" can be both a torque and an
                   angular acceleration and this lead to unit incompatibilities)
           </td>
</tr>

<tr><td colspan=\"2\"><b>Mechanics.Translational.</b></td></tr>
<tr><td valign=\"top\"> Speed</td>
          <td valign=\"top\"> Corrected unit of v_ref from SIunits.Position to SIunits.Velocity
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Examples.Tests.Components.</b></td></tr>
<tr><td valign=\"top\"> PartialTestModel<br>PartialTestModel2</td>
          <td valign=\"top\"> Corrected unit of h_start from \"SIunits.Density\" to \"SIunits.SpecificEnthalpy\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Examples.SolveOneNonlinearEquation.</b></td></tr>
<tr><td valign=\"top\"> Inverse_sh_T
                   InverseIncompressible_sh_T<br>
                   Inverse_sh_TX</td>
          <td valign=\"top\"> Rewrote equations so that dimensional (unit) analysis is correct\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Incompressible.Examples.</b></td></tr>
<tr><td valign=\"top\"> TestGlycol</td>
          <td valign=\"top\"> Rewrote equations so that dimensional (unit) analysis is correct\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Interfaces.PartialTwoPhaseMedium.</b></td></tr>
<tr><td valign=\"top\"> dBubbleDensity_dPressure<br>dDewDensity_dPressure</td>
          <td valign=\"top\"> Changed wrong type of ddldp from \"DerDensityByEnthalpy\"
                   to \"DerDensityByPressure\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Common.ThermoFluidSpecial.</b></td></tr>
<tr><td valign=\"top\"> ThermoProperties</td>
          <td valign=\"top\"> Changed wrong units:
                   \"SIunits.DerEnergyByPressure dupT\" to \"Real dupT\" and
                   \"SIunits.DerEnergyByDensity dudT\" to \"Real dudT\"
           </td>
</tr>
<tr><td valign=\"top\"> ThermoProperties_ph</td>
          <td valign=\"top\"> Changed wrong unit from \"SIunits.DerEnergyByPressure duph\" to \"Real duph\"
           </td>
</tr>
<tr><td valign=\"top\"> ThermoProperties_pT</td>
          <td valign=\"top\"> Changed wrong unit from \"SIunits.DerEnergyByPressure dupT\" to \"Real dupT\"
           </td>
</tr>
<tr><td valign=\"top\"> ThermoProperties_dT</td>
          <td valign=\"top\">  Changed wrong unit from \"SIunits.DerEnergyByDensity dudT\" to \"Real dudT\"
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.IdealGases.Common.SingleGasNasa.</b></td></tr>
<tr><td valign=\"top\"> cp_Tlow_der</td>
          <td valign=\"top\"> Changed wrong unit from \"SIunits.Temperature dT\" to \"Real dT\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.IF97_Utilities.BaseIF97.Basic.</b></td></tr>
<tr><td valign=\"top\"> p1_hs<br>
                   h2ab_s<br>
                   p2a_hs<br>
                   p2b_hs<br>
                   p2c_hs<br>
                   h3ab_p<br>
                   T3a_ph<br>
                   T3b_ph<br>
                   v3a_ph<br>
                   v3b_ph<br>
                   T3a_ps<br>
                   T3b_ps<br>
                   v3a_ps<br>
                   v3b_ps</td>
          <td valign=\"top\"> Changed wrong unit of variables h/hstar, s, sstar from
                   \"SIunits.Enthalpy\" to \"SIunits.SpecificEnthalpy\",
                   \"SIunits.SpecificEntropy\", \"SIunits.SpecificEntropy
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.IF97_Utilities.BaseIF97.Transport.</b></td></tr>
<tr><td valign=\"top\"> cond_dTp</td>
          <td valign=\"top\"> Changed wrong unit of TREL, rhoREL, lambdaREL from
                   \"SIunits.Temperature\", \"SIunit.Density\", \"SIunits.ThermalConductivity\"
                   to \"Real\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.IF97_Utilities.BaseIF97.Inverses.</b></td></tr>
<tr><td valign=\"top\"> tofps5<br>tofpst5</td>
          <td valign=\"top\"> Changed wrong unit of pros from \"SIunits.SpecificEnthalpy\" to
                   \"SIunits.SpecificEntropy\".
           </td>
</tr>

<tr><td colspan=\"2\"><b>Media.Water.IF97_Utilities.</b></td></tr>
<tr><td valign=\"top\"> waterBaseProp_ph</td>
          <td valign=\"top\"> Improved calculation at the limits of the validity.
           </td>
</tr>

        <tr><td colspan=\"2\"><b>Thermal.</b></td></tr>
<tr><td valign=\"top\"> FluidHeatFlow<br>HeatTransfer</td>
          <td valign=\"top\"> Corrected wrong unit \"SIunits.Temperature\" of difference temperature
                        variables with \"SIunits.TemperatureDifference\".
           </td>
</tr>

</table>

</html>"));
end Version_2_2_2;

class Version_2_2_1 "Version 2.2.1 (March 24, 2006)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p>
Version 2.2.1 is backward compatible to version 2.2.
</p>

<p>
In this version, <b>no</b> new libraries have been added.
The following major improvements have been made:
</p>

<ul>
<li> The <b>Documentation</b> of the Modelica standard library was
         considerably improved:<br>
         In Dymola 6, the new feature was introduced to automatically add tables
         for class content and component interface definitions (parameters and
         connectors) to the info layer. For this reason, the corresponding (partial)
         tables previously present in the Modelica Standard Library have been
         removed. The new feature of Dymola 6 has the significant advantage that
         all tables are now guaranteed to be up-to-date.<br>
         Additionally, the documentation has been improved by adding appropriate
         description texts to parameters, connector instances, function input
         and output arguments etc., in order that the automatically generated
         tables do not have empty entries. Also new User's Guides for sublibraries
         Rotational and SIunits have been added and the User's Guide on top
         level (Modelica.UsersGuide) has been improved.<br>&nbsp;</li>

<li> Initialization options have been added to the Modelica.Blocks.<b>Continuous</b>
         blocks (NoInit, SteadyState, InitialState, InitialOutput). If InitialOutput
         is selected, the block output is provided as initial condition. The states
         of the block are then initialized as close as possible to steady state.
         Furthermore, the Continuous.LimPID block has been significantly
         improved and much better documented.<br>&nbsp;</li>

<li> The Modelica.<b>Media</b> library has been significantly improved:<br>
         New functions setState_pTX, setState_phX, setState_psX, setState_dTX
         have been added to PartialMedium to compute the independent medium variables
         (= state of medium) from p,T,X, or from p,h,X or from p,s,X or from
         d,T,X. Then functions are provided for all interesting medium variables
         to compute them from its medium state. All these functions are
         implemented in a robust way for all media (with a few exceptions, if the
         generic function does not make sense for a particular medium).</li>
</ul>

<p>
The following <b>new components</b> have been added to <b>existing</b> libraries:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Examples.</b></td></tr>
<tr><td valign=\"top\"> PID_Controller</td>
          <td valign=\"top\"> Example to demonstrate the usage of the
                   Blocks.Continuous.LimPID block.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Math.</b></td></tr>
<tr><td valign=\"top\"> UnitConversions.*</td>
          <td valign=\"top\"> New package that provides blocks for unit conversions.
                   UnitConversions.ConvertAllBlocks allows to select all
                   available conversions from a menu.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.BasicMachines.SynchronousInductionMachines.</b></td></tr>
<tr><td valign=\"top\"> SM_ElectricalExcitedDamperCage</td>
          <td valign=\"top\"> Electrical excited synchronous induction machine with damper cage</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.BasicMachines.Components.</b></td></tr>
<tr><td valign=\"top\"> ElectricalExcitation </td>
          <td valign=\"top\"> Electrical excitation for electrical excited synchronous
                   induction machines</td></tr>
<tr><td valign=\"top\"> DamperCage</td>
          <td valign=\"top\"> Unsymmetrical damper cage for electrical excited synchronous
                   induction machines. At least the user has to specify the dampers
                   resistance and stray inductance in d-axis; if he omits the
                   parameters of the q-axis, the same values as for the d.axis
                   are used, assuming a symmetrical damper.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.Examples.</b></td></tr>
<tr><td valign=\"top\"> SMEE_Gen </td>
          <td valign=\"top\"> Test example 7: ElectricalExcitedSynchronousInductionMachine
                   as Generator</td></tr>
<tr><td valign=\"top\"> Utilities.TerminalBox</td>
          <td valign=\"top\"> Terminal box for three-phase induction machines to choose
                   either star (wye) ? or delta ? connection</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.</b></td></tr>
<tr><td valign=\"top\"> equalityLeastSquares</td>
          <td valign=\"top\"> Solve a linear equality constrained least squares problem:<br>
                  min|A*x-a|^2 subject to B*x=b</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\"> Parts.PointMass</td>
          <td valign=\"top\"> Point mass, i.e., body where inertia tensor is neglected.</td></tr>
<tr><td valign=\"top\"> Interfaces.FlangeWithBearing</td>
          <td valign=\"top\"> Connector consisting of 1-dim. rotational flange and its
                   3-dim. bearing frame.</td></tr>
<tr><td valign=\"top\"> Interfaces.FlangeWithBearingAdaptor</td>
          <td valign=\"top\"> Adaptor to allow direct connections to the sub-connectors
                   of FlangeWithBearing.</td></tr>
<tr><td valign=\"top\"> Types.SpecularCoefficient</td>
          <td valign=\"top\"> New type to define a specular coefficient.</td></tr>
<tr><td valign=\"top\"> Types.ShapeExtra</td>
          <td valign=\"top\"> New type to define the extra data for visual shape objects and to
                   have a central place for the documentation of this data.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Examples.Elementary</b></td></tr>
<tr><td valign=\"top\"> PointGravityWithPointMasses</td>
          <td valign=\"top\"> Example of two point masses in a central gravity field.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\">UsersGuide</td>
          <td valign=\"top\"> A User's Guide has been added by using the documentation previously
                   present in the package documentation of Rotational.</td></tr>
<tr><td valign=\"top\">Sensors.PowerSensor</td>
          <td valign=\"top\"> New component to measure the energy flow between two connectors
                   of the Rotational library.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Translational.</b></td></tr>
<tr><td valign=\"top\">Speed</td>
          <td valign=\"top\"> New component to move a translational flange
                   according to a reference speed</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Media.Interfaces.PartialMedium.</b></td></tr>
<tr><td valign=\"top\">specificEnthalpy_pTX</td>
          <td valign=\"top\"> New function to compute specific enthalpy from pressure, temperature
                   and mass fractions.</td></tr>
<tr><td valign=\"top\">temperature_phX</td>
          <td valign=\"top\"> New function to compute temperature from pressure, specific enthalpy,
                   and mass fractions.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Icons.</b></td></tr>
<tr><td valign=\"top\"> SignalBus</td>
          <td valign=\"top\"> Icon for signal bus</td></tr>
<tr><td valign=\"top\"> SignalSubBus</td>
          <td valign=\"top\"> Icon for signal sub-bus</td></tr>

<tr><td colspan=\"2\"><b>Modelica.SIunits.</b></td></tr>
<tr><td valign=\"top\">UsersGuide</td>
          <td valign=\"top\"> A User's Guide has been added that describes unit handling.</td></tr>
<tr><td valign=\"top\"> Resistance<br>
                   Conductance</td>
          <td valign=\"top\"> Attribute 'min=0' removed from these types.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Thermal.FluidHeatFlow.</b></td></tr>
<tr><td valign=\"top\"> Components.Valve</td>
          <td valign=\"top\"> Simple controlled valve with either linear or
                   exponential characteristic.</td></tr>
<tr><td valign=\"top\"> Sources. IdealPump </td>
          <td valign=\"top\"> Simple ideal pump (resp. fan)  dependent on the shaft's speed;
                   pressure increase versus volume flow is defined as a linear
                   function. Torque * Speed = Pressure increase * Volume flow
                   (without losses).</td></tr>
<tr><td valign=\"top\"> Examples.PumpAndValve </td>
          <td valign=\"top\"> Test example for valves.</td></tr>
<tr><td valign=\"top\"> Examples.PumpDropOut </td>
          <td valign=\"top\"> Drop out of 1 pump to test behavior of semiLinear.</td></tr>
<tr><td valign=\"top\"> Examples.ParallelPumpDropOut </td>
          <td valign=\"top\"> Drop out of 2 parallel pumps to test behavior of semiLinear.</td></tr>
<tr><td valign=\"top\"> Examples.OneMass </td>
          <td valign=\"top\"> Cooling of 1 hot mass to test behavior of semiLinear.</td></tr>
<tr><td valign=\"top\"> Examples.TwoMass </td>
          <td valign=\"top\"> Cooling of 2 hot masses to test behavior of semiLinear.</td></tr>
</table>

<p>
The following <b>components</b> have been improved:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.</b></td></tr>
<tr><td valign=\"top\"> UsersGuide</td>
          <td valign=\"top\"> User's Guide and package description of Modelica Standard Library improved.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Interfaces.</b></td></tr>
<tr><td valign=\"top\"> RealInput<br>
                   BooleanInput<br>
                   IntegerInput</td>
          <td valign=\"top\"> When dragging one of these connectors the width and height
                   is a factor of 2 larger as a standard icon. Previously,
                   these connectors have been dragged and then manually enlarged
                   by a factor of 2 in the Modelica standard library.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.</b></td></tr>
<tr><td valign=\"top\"> Continuous.*</td>
          <td valign=\"top\"> Initialization options added to all blocks
                   (NoInit, SteadyState, InitialState, InitialOutput).
                   New parameter limitsAtInit to switch off the limits
                   of LimIntegrator or LimPID during initialization</td></tr>
<tr><td valign=\"top\"> Continuous.LimPID</td>
          <td valign=\"top\"> Option to select P, PI, PD, PID controller.
                   Documentation significantly improved.</td></tr>
<tr><td valign=\"top\"> Nonlinear.Limiter<br>
                   Nonlinear.VariableLimiter<br>
                   Nonlinear.Deadzone</td>
          <td valign=\"top\"> New parameter limitsAtInit/deadZoneAtInit to switch off the limits
                   or the dead zone during initialization</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog. </b></td></tr>
<tr><td valign=\"top\"> Sources</td>
          <td valign=\"top\"> Icon improved (+/- added to voltage sources, arrow added to
                   current sources).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Semiconductors. </b></td></tr>
<tr><td valign=\"top\"> Diode</td>
          <td valign=\"top\"> smooth() operator included to improve numerics.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.BasicMachines.SynchronousInductionMachines. </b></td></tr>
<tr><td valign=\"top\"> SM_PermanentMagnetDamperCage<br>
                   SM_ElectricalExcitedDamperCage<br>
                   SM_ReluctanceRotorDamperCage</td>
          <td valign=\"top\"> The user can choose \"DamperCage = false\" (default: true)
                   to remove all equations for the damper cage from the model.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Machines.BasicMachines.AsynchronousInductionMachines. </b></td></tr>
<tr><td valign=\"top\"> AIM_SlipRing</td>
          <td valign=\"top\"> Easier parameterization: if the user selects \"useTurnsRatio = false\"
                   (default: true, this is the same behavior as before),
                        parameter TurnsRatio is calculated internally from
                        Nominal stator voltage and Locked-rotor voltage.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Math.Matrices.</b></td></tr>
<tr><td valign=\"top\">leastSquares</td>
          <td valign=\"top\">The A matrix in the least squares problem might be rank deficient.
                  Previously, it was required that A has full rank.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\">all models</td>
          <td valign=\"top\"> <ul>
                   <li> All components with animation information have a new variable
                                <b>specularCoefficient</b> to define the reflection of ambient light.
                                The default value is world.defaultSpecularCoefficient which has
                                a default of 0.7. By changing world.defaultSpecularCoefficient, the
                                specularCoefficient of all components is changed that are not
                                explicitly set differently. Since specularCoefficient is a variable
                                (and no parameter), it can be changed during simulation. Since
                                annotation(Dialog) is set, this variable still appears in the
                                parameter menus.<br>
                                Previously, a constant specularCoefficient of 0.7 was used
                                for all components.</li>
                   <li> Variable <b>color</b> of all components is no longer a parameter
                                but an input variable. Also all parameters in package <b>Visualizers</b>,
                                with the exception of <b>shapeType</b> are no longer parameters but
                                defined as input variables with annotation(Dialog). As a result,
                                all these variables appear still in parameter menus, but they can
                                be changed during simulation (e.g., color might be used to
                                display the temperature of a part).</li>
                   <li> All menus have been changed to follow the Modelica 2.2 annotations
                                \"Dialog, group, tab, enable\" (previously, a non-standard Dymola
                                definition for menus was used). Also, the \"enable\" annotation
                                is used in all menus
                                to disable input fields if the input would be ignored.</li>
                   <li> All visual shapes are now defined with conditional declarations
                                (to remove them, if animation is switched off). Previously,
                                these (protected) objects have been defined by arrays with
                                dimension 0 or 1.</li>
                   </ul></td></tr>

<tr><td valign=\"top\">Frames.resolveRelative</td>
          <td valign=\"top\"> The derivative of this function added as function and defined via
                   an annotation. In certain situations, tools had previously
                   difficulties to differentiate the inlined function automatically.</td></tr>

<tr><td valign=\"top\">Forces.*</td>
          <td valign=\"top\"> The scaling factors N_to_m and Nm_to_m have no longer a default
                   value of 1000 but a default value of world.defaultN_to_m (=1000)
                   and world.defaultNm_to_m (=1000). This allows to change the
                   scaling factors for all forces and torques in the world
                   object.</td></tr>
<tr><td valign=\"top\">Interfaces.Frame.a<br>
                  Interfaces.Frame.b<br>
                  Interfaces.Frame_resolve</td>
          <td valign=\"top\"> The Frame connectors are now centered around the origin to ease
                   the usage. The shape was changed, such that the icon is a factor
                   of 1.6 larger as a standard icon (previously, the icon had a
                   standard size when dragged and then the icon was manually enlarged
                   by a factor of 1.5 in the y-direction in the MultiBody library;
                   the height of 16 allows easy positioning on the standard grid size of 2).
                   The double line width of the border in icon and diagram layer was changed
                   to a single line width and when making a connection the connection
                   line is dark grey and no longer black which looks better.</td></tr>
<tr><td valign=\"top\">Joints.Assemblies.*</td>
          <td valign=\"top\"> When dragging an assembly joint, the icon is a factor of 2
                   larger as a standard icon. Icon texts and connectors have a
                   standard size in this enlarged icon (and are not a factor of 2
                   larger as previously).</td></tr>
<tr><td valign=\"top\">Types.*</td>
          <td valign=\"top\"> All types have a corresponding icon now to visualize the content
                   in the package browser (previously, the types did not have an icon).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\"> Inertia</td>
          <td valign=\"top\"> Initialization and state selection added.</td></tr>
<tr><td valign=\"top\"> SpringDamper</td>
          <td valign=\"top\"> Initialization and state selection added.</td></tr>
<tr><td valign=\"top\"> Move</td>
          <td valign=\"top\"> New implementation based solely on Modelica 2.2 language
                   (previously, the Dymola specific constrain(..) function was used).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Mechanics.Translational.</b></td></tr>
<tr><td valign=\"top\"> Move</td>
          <td valign=\"top\"> New implementation based solely on Modelica 2.2 language
                   (previously, the Dymola specific constrain(..) function was used).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Thermal.FluidHeatFlow.Interfaces.Partials.</b></td></tr>
<tr><td valign=\"top\"> SimpleFriction</td>
          <td valign=\"top\"> Calculates friction losses from pressure drop and volume flow.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Thermal.FluidHeatFlow.Components.</b></td></tr>
<tr><td valign=\"top\"> IsolatedPipe <br>
                   HeatedPipe</td>
          <td valign=\"top\"> Added geodetic height as a source of pressure change;
                   feeds friction losses as calculated by simple friction to
                   the energy balance of the medium.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Media.Interfaces.PartialMedium.FluidConstants.</b></td></tr>
<tr><td valign=\"top\">HCRIT0</td><td valign=\"top\">Critical specific enthalpy of the fundamental
                  equation (base formulation of the fluid medium model).</td></tr>
<tr><td valign=\"top\">SCRIT0</td><td valign=\"top\">Critical specific entropy of the fundamental
                  equation (base formulation of the fluid medium model).</td></tr>
<tr><td valign=\"top\">deltah</td><td valign=\"top\">Enthalpy offset (default: 0) between the
                  specific enthalpy of the fluid model and the user-visible
                  specific enthalpy in the model: deltah = h_model - h_fundamentalEquation.
</td></tr>
<tr><td valign=\"top\">deltas</td><td valign=\"top\">Entropy offset (default: 0) between the
                  specific entropy of the fluid model and the user-visible
                  specific entropy in the model: deltas = s_model - s_fundamentalEquation.</td></tr>
<tr><td valign=\"top\">T_default</td><td valign=\"top\">Default value for temperature of medium (for initialization)</td></tr>
<tr><td valign=\"top\">h_default</td><td valign=\"top\">Default value for specific enthalpy of medium (for initialization)</td></tr>
<tr><td valign=\"top\">p_default</td><td valign=\"top\">Default value for pressure of medium (for initialization)</td></tr>
<tr><td valign=\"top\">X_default</td><td valign=\"top\">Default value for mass fractions of medium (for initialization)</td></tr>
</table>
<p>
The following <b>errors</b> have been fixed:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Tables.</b></td></tr>
<tr><td valign=\"top\">CombiTable1D<br>
                  CombiTable1Ds<br>
                  CombiTable2D</td>
          <td valign=\"top\"> Parameter \"tableOnFile\" determines now whether a table is read from
                   file or used from parameter \"table\". Previously, if \"fileName\" was not
                   \"NoName\", a table was always read from file \"fileName\", independently
                   of the setting of \"tableOnFile\". This has been corrected.<br>
                   Furthermore, the initialization of a table is now performed in a
                   when-clause and no longer in a parameter declaration, because some
                   tools evaluate the parameter declaration in some situation more than
                   once and then the table is unnecessarily read several times
                   (and occupies also more memory).</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Blocks.Sources.</b></td></tr>
<tr><td valign=\"top\">CombiTimeTable</td>
          <td valign=\"top\"> Same bug fix/improvement as for the tables from Modelica.Blocks.Tables
                   as outlined above.</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Semiconductors. </b></td></tr>
<tr><td valign=\"top\"> PMOS<br>
                   NMOS<br>
                   HeatingPMOS<br>
                   HeatingNMOS</td>
          <td valign=\"top\"> The Drain-Source-Resistance RDS had actually a resistance of
                   RDS/v, with v=Beta*(W+dW)/(L+dL). The correct formula is without
                   the division by \"v\". This has now been corrected.<br>
                   This bug fix should not have an essential effect in most applications.
                   In the default case (Beta=1e-5), the Drain-Source-Resistance was
                   a factor of 1e5 too large and had in the default case the
                   wrong value 1e12, although it should have the value 1e7. The effect
                   was that this resistance had practically no effect.</td></tr>

<tr><td colspan=\"2\"> <b>Modelica.Media.IdealGases.Common.SingleGasNasa.</b></td></tr>
<tr><td valign=\"top\"> dynamicViscosityLowPressure</td>
          <td valign=\"top\"> Viscosity and thermal conductivity (which needs viscosity as input)
                   were computed wrong for polar gases and gas mixtures
                   (i.e., if dipole moment not 0.0). This has been fixed in version 2.2.1.</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Utilities.Streams.</b></td></tr>
<tr><td valign=\"top\">readLine</td>
          <td valign=\"top\"> Depending on the C-implementation, the stream was not correctly closed.
                   This has been corrected by adding a \"Streams.close(..)\"
                   after reading the file content.</td></tr>

</table>
</html>"));
end Version_2_2_1;

class Version_2_2 "Version 2.2 (April 6, 2005)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p>
Version 2.2 is backward compatible to version 2.1.
</p>

<p>
The following <b>new libraries</b> have been added:
</p>

<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Media\">Modelica.Media</a></td>
          <td valign=\"top\"> Property models of liquids and gases, especially
                   <ul>
                   <li>1241 detailed gas models,</li>
                   <li> moist air,</li>
                   <li> high precision water model (according to IAPWS/IF97 standard), </li>
                   <li> incompressible media defined by tables (cp(T), rho(t), eta(T), etc. are defined by tables).</li>
                   </ul>
                   The user can conveniently define mixtures of gases between the
                   1241 gas models. The models are
                   designed to work well in dynamic simulations. They
                   are based on a new standard interface for media with
                   single and multiple substances and one or multiple phases
                   with the following features:
                   <ul>
                   <li> The independent variables of a medium model do not influence the
                                definition of a fluid connector port or how the
                                balance equations have to be implemented.<br>
                                Used independent variables: \"p,T\", \"p,T,X\", \"p,h\", \"d,T\".</li>
                   <li> Optional variables, e.g., dynamic viscosity, are only computed
                                if needed.</li>
                   <li> The medium models are implemented with regards to efficient
                                dynamic simulation.</li>
                   </ul>
          </td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Thermal.FluidHeatFlow\">Modelica.Thermal.FluidHeatFlow</a></td>
          <td valign=\"top\"> Simple components for 1-dim., incompressible thermo-fluid flow
                   to model coolant flows, e.g., of electrical machines.
                   Components can be connected arbitrarily together (= ideal mixing
                   at connection points) and fluid may reverse direction of flow.
</td></tr>
</table>
<p>
The following <b>changes</b> have been performed in the
<b>Modelica.Mechanics.MultiBody</b> library:
</p>
<ul>
<li> Component MultiBody.World has a new parameter
         <b>driveTrainMechanics3D</b>. If set to <b>true</b>, 3D mechanical effects
         of MultiBody.Parts.Mounting1D/Rotor1D/BevelGear1D are taken into account. If set to
         <b>false</b> (= default), 3D mechanical effects in these elements
         are not taken into account and the
         frame connectors to connect to 3D parts are disabled (all
         connections to such a disabled connector are also disabled, due to the
         new feature of conditional declarations in Modelica language 2.2)</li>
<li> All references to \"MultiBody.xxx\" have
         been changed to \"Modelica.Mechanics.MultiBodys.xxx\" in order that after
         copying of a component outside of the Modelica library, the references
         still remain valid.</li>
</ul>
</html>"));
end Version_2_2;

class Version_2_1 "Version 2.1 (Nov. 11, 2004)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p> This is a major change with respect to previous versions of the
        Modelica Standard Library, because <b>many new libraries</b> and components
        are included and because the input/output blocks (Modelica.Blocks)
        have been considerably simplified:
</p>
<ul>
<li> An input/output connector is defined <b>without</b> a hierarchy (this is possible
         due to new features of the Modelica language). For example, the input
         signal of a block \"FirstOrder\" was previously accessed as \"FirstOrder.inPort.signal[1]\".
         Now it is accessed as \"FirstOrder.u\". This simplifies the understanding and usage
         especially for beginners.</li>
<li> De-vectorized the <b>Modelica.Blocks</b> library. All blocks in the
         Modelica.Blocks library are now scalar blocks. As a result,
         the parameters of the Blocks are scalars and no vectors any
         more. For example, a parameter \"amplitude\" that might had
         a value of \"{1}\" previously, has now a value of \"1\". This simplifies
         the understanding and usage especially for beginners.<br>
         If a vector of blocks is needed, this can be easily
         accomplished by adding a dimension to the instance. For example
         \"Constant const[3](k={1,2,3}\" defines three Constant blocks.
         An additional advantage of the new approach is that
         the implementation of Modelica.Blocks is much simpler and is easier to
         understand.
</li>
</ul>

<p>
The discussed changes of Modelica.Blocks are not backward compatible.
A script to <b>automatically</b> convert models to this new version is
provided. There might be rare cases, where this script does not convert.
In this case models have to be manually converted.
In any case you should make a back-up copy of your model
before automatic conversion is performed.
</p>
<p>
The following <b>new libraries</b> have been added:
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Digital\">Modelica.Electrical.Digital</a></td>
          <td valign=\"top\">Digital electrical components based on 2-,3-,4-, and 9-valued logic<br>
                  according to the VHDL standard</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Machines\">Modelica.Electrical.Machines</a></td>
          <td valign=\"top\">Asynchronous, synchronous and DC motor and generator models</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Math.Matrices\">Modelica.Math.Matrices</a></td>
          <td valign=\"top\">Functions operating on matrices such as solve() (A*x=b), leastSquares(),<br>
                  norm(), LU(), QR(),  eigenValues(), singularValues(), exp(), ...</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.StateGraph\">Modelica.StateGraph</a></td>
          <td valign=\"top\"> Modeling of <b>discrete event</b> and <b>reactive</b> systems in a convenient way using<br>
                   <b>hierarchical state machines</b> and <b>Modelica</b> as <b>action language</b>. <br>
                   It is based on JGrafchart and Grafcet and  has a similar modeling power as <br>
                   StateCharts. It avoids deficiencies of usually used action languages. <br>
                   This library makes the ModelicaAdditions.PetriNets library obsolete.</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Utilities.Files\">Modelica.Utilities.Files</a></td>
          <td valign=\"top\">Functions to operate on files and directories (copy, move, remove files etc.)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Utilities.Streams\">Modelica.Utilities.Streams</a></td>
          <td valign=\"top\">Read from files and write to files (print, readLine, readFile, error, ...)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Utilities.Strings\">Modelica.Utilities.Strings</a></td>
          <td valign=\"top\">Operations on strings (substring, find, replace, sort, scanToken, ...)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Utilities.System\">Modelica.Utilities.System</a></td>
          <td valign=\"top\">Get/set current directory, get/set environment variable, execute shell command, etc.</td></tr>
</table>
<p>
The following existing libraries outside of the Modelica standard library
have been improved and added as <b>new libraries</b>
(models using the previous libraries are automatically converted
to the new sublibraries inside package Modelica):
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Blocks.Discrete\">Modelica.Blocks.Discrete</a></td>
          <td valign=\"top\"> Discrete input/output blocks with fixed sample period<br>
                   (from ModelicaAdditions.Blocks.Discrete)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Blocks.Logical\">Modelica.Blocks.Logical</a></td>
          <td valign=\"top\"> Logical components with Boolean input and output signals<br>
                   (from ModelicaAdditions.Blocks.Logical)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Blocks.Nonlinear\">Modelica.Blocks.Nonlinear</a></td>
          <td valign=\"top\"> Discontinuous or non-differentiable algebraic control blocks such as variable limiter,<br>
                   fixed, variable and Pade delay etc. (from ModelicaAdditions.Blocks.Nonlinear)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Blocks.Routing\">Modelica.Blocks.Routing</a></td>
          <td valign=\"top\"> Blocks to combine and extract signals, such as multiplexer<br>
                   (from ModelicaAdditions.Blocks.Multiplexer)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Blocks.Tables\">Modelica.Blocks.Tables</a></td>
          <td valign=\"top\"> One and two-dimensional interpolation in tables. CombiTimeTable is available<br>
                   in Modelica.Blocks.Sources (from ModelicaAdditions.Tables)</td></tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.MultiBody\">Modelica.Mechanics.MultiBody</a></td>
          <td valign=\"top\"> Components to model the movement of 3-dimensional mechanical systems. Contains <br>
                   body, joint, force and sensor components, analytic handling of kinematic loops,<br>
                   force elements with mass, series/parallel connection of 3D force elements etc.<br>
                   (from MultiBody 1.0 where the new signal connectors are used;<br>
                   makes the ModelicaAdditions.MultiBody library obsolete)</td></tr>
</table>
<p>
As a result, the ModelicaAdditions library is obsolete, because all components
are either included in the Modelica library or are replaced by much more
powerful libraries (MultiBody, StateGraph).
</p>
<p>
The following <b>new components</b> have been added to <b>existing</b> libraries.
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Logical.</b></td></tr>
<tr><td valign=\"top\">Pre</td>
          <td valign=\"top\">y = pre(u): Breaks algebraic loops by an infinitesimal small <br>
                  time delay (event iteration continues until u = pre(u))</td></tr>
<tr><td valign=\"top\">Edge</td>
          <td valign=\"top\">y = edge(u): Output y is true, if the input u has a rising edge </td></tr>
<tr><td valign=\"top\">FallingEdge</td>
          <td valign=\"top\">y = edge(not u): Output y is true, if the input u has a falling edge </td></tr>
<tr><td valign=\"top\">Change</td>
          <td valign=\"top\">y = change(u): Output y is true, if the input u has a rising or falling edge </td></tr>
<tr><td valign=\"top\">GreaterEqual</td>
          <td valign=\"top\">Output y is true, if input u1 is greater or equal than input u2 </td></tr>
<tr><td valign=\"top\">Less</td>
          <td valign=\"top\">Output y is true, if input u1 is less than input u2 </td></tr>
<tr><td valign=\"top\">LessEqual</td>
          <td valign=\"top\">Output y is true, if input u1 is less or equal than input u2 </td></tr>
<tr><td valign=\"top\">Timer</td>
          <td valign=\"top\">Timer measuring the time from the time instant where the <br>
                  Boolean input became true </td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Math.</b></td></tr>
<tr><td valign=\"top\">BooleanToReal</td>
          <td valign=\"top\">Convert Boolean to Real signal</td></tr>
<tr><td valign=\"top\">BooleanToInteger</td>
          <td valign=\"top\">Convert Boolean to Integer signal</td></tr>
<tr><td valign=\"top\">RealToBoolean</td>
          <td valign=\"top\">Convert Real to Boolean signal</td></tr>
<tr><td valign=\"top\">IntegerToBoolean</td>
          <td valign=\"top\">Convert Integer to Boolean signal</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Sources.</b></td></tr>
<tr><td valign=\"top\">RealExpression</td>
          <td valign=\"top\">Set output signal to a time varying Real expression</td></tr>
<tr><td valign=\"top\">IntegerExpression</td>
          <td valign=\"top\">Set output signal to a time varying Integer expression</td></tr>
<tr><td valign=\"top\">BooleanExpression</td>
          <td valign=\"top\">Set output signal to a time varying Boolean expression</td></tr>
<tr><td valign=\"top\">BooleanTable</td>
          <td valign=\"top\">Generate a Boolean output signal based on a vector of time instants</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.</b></td></tr>
<tr><td valign=\"top\">Frames.from_T2</td>
          <td valign=\"top\">Return orientation object R from transformation matrix T and its derivative der(T)</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\">LinearSpeedDependentTorque</td>
          <td valign=\"top\">Linear dependency of torque versus speed (acts as load torque)</td></tr>
<tr><td valign=\"top\">QuadraticSpeedDependentTorque</td>
          <td valign=\"top\">Quadratic dependency of torque versus speed (acts as load torque)</td></tr>
<tr><td valign=\"top\">ConstantTorque</td>
          <td valign=\"top\">Constant torque, not dependent on speed (acts as load torque)</td></tr>
<tr><td valign=\"top\">ConstantSpeed</td>
          <td valign=\"top\">Constant speed, not dependent on torque (acts as load torque)</td></tr>
<tr><td valign=\"top\">TorqueStep</td>
          <td valign=\"top\">Constant torque, not dependent on speed (acts as load torque)</td></tr>
</table>
<p>
The following <b>bugs</b> have been <b>corrected</b>:
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.MultiBody.Forces.</b></td></tr>
<tr><td valign=\"top\">LineForceWithMass<br>
                  Spring</td>
          <td valign=\"top\">If mass of the line force or spring component is not zero, the<br>
                  mass was (implicitly) treated as \"mass*mass\" instead of as \"mass\"</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\">Speed</td>
          <td valign=\"top\">If parameter exact=<b>false</b>, the filter was wrong<br>
                  (position was filtered and not the speed input signal)</td></tr>
</table>
<p>
Other changes:
</p>
<ul>
<li> All connectors are now smaller in the diagram layer. This gives
         a nicer layout when connectors and components are used together
         in a diagram</li>
<li> Default instance names are defined for all connectors, according
         to a new annotation introduced in Modelica 2.1. For example,
         when dragging connector \"Flange_a\" from the Rotational library to
         the diagram layer, the default connector instance name is
         \"flange_a\" and not \"Flange_a1\".</li>
<li> The Modelica.Mechanics.Rotational connectors are changed from
         a square to a circle</li>
<li> The Modelica.Mechanics.Translational connectors are changed from a
         green to a dark green color in order that connection lines
         can be better seen, especially when printed.</li>
<li> The Modelica.Blocks connectors for Real signals are changed from
         blue to dark blue in order to distinguish them from electrical signals.</li>
</ul>
</html>"));
end Version_2_1;

class Version_1_6 "Version 1.6 (June 21, 2004)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p> Added 1 new library (Electrical.MultiPhase), 17 new components,
        improved 3 existing components
        in the Modelica.Electrical library and improved 3 types
        in the Modelica.SIunits library. Furthermore,
        this User's Guide has been started. The improvements
        in more detail:
</p>
<p>
<b>New components</b>
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Basic.</b></td></tr>
<tr><td valign=\"top\">SaturatingInductor</td>
          <td valign=\"top\">Simple model of an inductor with saturation</td></tr>
<tr><td valign=\"top\">VariableResistor</td>
          <td valign=\"top\">Ideal linear electrical resistor with variable resistance</td></tr>
<tr><td valign=\"top\">VariableConductor</td>
          <td valign=\"top\">Ideal linear electrical conductor with variable conductance</td></tr>
<tr><td valign=\"top\">VariableCapacitor</td>
          <td valign=\"top\">Ideal linear electrical capacitor with variable capacitance</td></tr>
<tr><td valign=\"top\">VariableInductor</td>
          <td valign=\"top\">Ideal linear electrical inductor with variable inductance</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Semiconductors.</b></td></tr>
<tr><td valign=\"top\">HeatingDiode</td>
          <td valign=\"top\">Simple diode with heating port</td></tr>
<tr><td valign=\"top\">HeatingNMOS</td>
          <td valign=\"top\">Simple MOS Transistor with heating port</td></tr>
<tr><td valign=\"top\">HeatingPMOS</td>
          <td valign=\"top\">Simple PMOS Transistor with heating port</td></tr>
<tr><td valign=\"top\">HeatingNPN</td>
          <td valign=\"top\">Simple NPN BJT according to Ebers-Moll with heating port</td></tr>
<tr><td valign=\"top\">HeatingPNP</td>
          <td valign=\"top\">Simple PNP BJT according to Ebers-Moll with heating port</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.MultiPhase</b><br>
          A new library for multi-phase electrical circuits</td></tr>
</table>
<p>
<b>New examples</b>
</p>
<p>
The following new examples have been added to
Modelica.Electrical.Analog.Examples:
</p>
<p>
CharacteristicThyristors,
CharacteristicIdealDiodes,
HeatingNPN_OrGate,
HeatingMOSInverter,
HeatingRectifier,
Rectifier,
ShowSaturatingInductor
ShowVariableResistor
</p>
<p>
<b>Improved existing components</b>
</p>
<p>In the library Modelica.Electrical.Analog.Ideal,
a knee voltage has been introduced for the components
IdealThyristor, IdealGTOThyristor, IdealDiode in order
that the approximation of these ideal elements is improved
with not much computational effort.</p>
<p> In the Modelica.SIunits library, the following changes
        have been made:</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td valign=\"top\">Inductance</td>
          <td valign=\"top\">min=0 removed</td></tr>
<tr><td valign=\"top\">SelfInductance</td>
          <td valign=\"top\">min=0 added</td></tr>
<tr><td valign=\"top\">ThermodynamicTemperature</td>
          <td valign=\"top\">min=0 added</td></tr>
</table>
</html>"));
end Version_1_6;

class Version_1_5 "Version 1.5 (Dec. 16, 2002)"
  extends Modelica.Icons.ReleaseNotes;

   annotation (Documentation(info="<html>

<p> Added 55 new components. In particular, added new package
        <b>Thermal.HeatTransfer</b> for modeling of lumped
        heat transfer, added model <b>LossyGear</b> in Mechanics.Rotational
        to model gear efficiency and bearing friction according to a new
        theory in a robust way, added 10 new models in Electrical.Analog and
        added several other new models and improved existing models.
</p>
<p>
<b>New components</b>
</p>
<table border=\"1\" cellspacing=0 cellpadding=2>
<tr><td colspan=\"2\"><b>Modelica.Blocks.</b></td></tr>
<tr><td valign=\"top\">Continuous.Der</td><td valign=\"top\">Derivative of input (= analytic differentiations)</td></tr>
<tr><td valign=\"top\"><b><i>Examples</i></b></td><td valign=\"top\">Demonstration examples of the components of this package</td></tr>
<tr><td valign=\"top\">Nonlinear.VariableLimiter</td><td valign=\"top\">Limit the range of a signal with variable limits</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Interfaces.</b></td></tr>
<tr><td valign=\"top\">RealPort</td><td valign=\"top\">Real port (both input/output possible)</td></tr>
<tr><td valign=\"top\">IntegerPort</td><td valign=\"top\">Integer port (both input/output possible)</td></tr>
<tr><td valign=\"top\">BooleanPort</td><td valign=\"top\">Boolean port (both input/output possible)</td></tr>
<tr><td valign=\"top\">SIMO</td><td valign=\"top\">Single Input Multiple Output continuous control block</td></tr>
<tr><td valign=\"top\">IntegerBlockIcon</td><td valign=\"top\">Basic graphical layout of Integer block</td></tr>
<tr><td valign=\"top\">IntegerMO</td><td valign=\"top\">Multiple Integer Output continuous control block</td></tr>
<tr><td valign=\"top\">IntegerSignalSource</td><td valign=\"top\">Base class for continuous Integer signal source</td></tr>
<tr><td valign=\"top\">IntegerMIBooleanMOs</td><td valign=\"top\">Multiple Integer Input Multiple Boolean Output continuous control block with same number of inputs and outputs</td></tr>
<tr><td valign=\"top\">BooleanMIMOs</td><td valign=\"top\">Multiple Input Multiple Output continuous control block with same number of inputs and outputs of Boolean type</td></tr>
<tr><td valign=\"top\"><b><i>BusAdaptors</i></b></td><td valign=\"top\">Components to send signals to the bus or receive signals from the bus</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Math.</b></td></tr>
<tr><td valign=\"top\">RealToInteger</td><td valign=\"top\">Convert real to integer signals</td></tr>
<tr><td valign=\"top\">IntegerToReal</td><td valign=\"top\">Convert integer to real signals</td></tr>
<tr><td valign=\"top\">Max</td><td valign=\"top\">Pass through the largest signal</td></tr>
<tr><td valign=\"top\">Min</td><td valign=\"top\">Pass through the smallest signal</td></tr>
<tr><td valign=\"top\">Edge</td><td valign=\"top\">Indicates rising edge of Boolean signal</td></tr>
<tr><td valign=\"top\">BooleanChange</td><td valign=\"top\">Indicates Boolean signal changing</td></tr>
<tr><td valign=\"top\">IntegerChange</td><td valign=\"top\">Indicates integer signal changing</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Blocks.Sources.</b></td></tr>
<tr><td valign=\"top\">IntegerConstant</td><td valign=\"top\">Generate constant signals of type Integer</td></tr>
<tr><td valign=\"top\">IntegerStep</td><td valign=\"top\">Generate step signals of type Integer</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Basic.</b></td></tr>
<tr><td valign=\"top\">HeatingResistor</td><td valign=\"top\">Temperature dependent electrical resistor</td></tr>
<tr><td valign=\"top\">OpAmp</td><td valign=\"top\">Simple nonideal model of an OpAmp with limitation</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Ideal.</b></td></tr>
<tr><td valign=\"top\">IdealCommutingSwitch</td><td valign=\"top\">Ideal commuting switch</td></tr>
<tr><td valign=\"top\">IdealIntermediateSwitch</td><td valign=\"top\">Ideal intermediate switch</td></tr>
<tr><td valign=\"top\">ControlledIdealCommutingSwitch</td><td valign=\"top\">Controlled ideal commuting switch</td></tr>
<tr><td valign=\"top\">ControlledIdealIntermediateSwitch</td><td valign=\"top\">Controlled ideal intermediate switch</td></tr>
<tr><td valign=\"top\">IdealOpAmpLimited</td><td valign=\"top\">Ideal operational amplifier with limitation</td></tr>
<tr><td valign=\"top\">IdealOpeningSwitch</td><td valign=\"top\">Ideal opener</td></tr>
<tr><td valign=\"top\">IdealClosingSwitch</td><td valign=\"top\">Ideal closer</td></tr>
<tr><td valign=\"top\">ControlledIdealOpeningSwitch</td><td valign=\"top\">Controlled ideal opener</td></tr>
<tr><td valign=\"top\">ControlledIdealClosingSwitch</td><td valign=\"top\">Controlled ideal closer</td></tr>

<tr><td colspan=\"2\"><b>Modelica.Electrical.Analog.Lines.</b></td></tr>
<tr><td valign=\"top\">TLine1</td><td valign=\"top\">Lossless transmission line (Z0, TD)</td></tr>
<tr><td valign=\"top\">TLine2</td><td valign=\"top\">Lossless transmission line (Z0, F, NL)</td></tr>
<tr><td valign=\"top\">TLine2</td><td valign=\"top\">Lossless transmission line (Z0, F)</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Icons.</b></td></tr>
<tr><td valign=\"top\">Function</td><td valign=\"top\">Icon for a function</td></tr>
<tr><td valign=\"top\">Record</td><td valign=\"top\">Icon for a record</td></tr>
<tr><td valign=\"top\">Enumeration</td><td valign=\"top\">Icon for an enumeration</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Math.</b></td></tr>
<tr><td valign=\"top\">tempInterpol2</td><td valign=\"top\">temporary routine for vectorized linear interpolation (will be removed)</td></tr>
<tr><td colspan=\"2\"><b>Modelica.Mechanics.Rotational.</b></td></tr>
<tr><td valign=\"top\">Examples.LossyGearDemo1</td><td valign=\"top\">Example to show that gear efficiency may lead to stuck motion</td></tr>
<tr><td valign=\"top\">Examples.LossyGearDemo2</td><td valign=\"top\">Example to show combination of LossyGear and BearingFriction</td></tr>
<tr><td valign=\"top\">LossyGear</td><td valign=\"top\">Gear with mesh efficiency and bearing friction (stuck/rolling possible)</td></tr>
<tr><td valign=\"top\">Gear2</td><td valign=\"top\">Realistic model of a gearbox (based on LossyGear)</td></tr>
<tr><td colspan=\"2\"><b>Modelica.SIunits.</b></td></tr>
<tr><td valign=\"top\"><b><i>Conversions</i></b></td><td valign=\"top\">Conversion functions to/from non SI units and type definitions of non SI units</td></tr>
<tr><td valign=\"top\">EnergyFlowRate</td><td valign=\"top\">Same definition as <i>Power</i></td></tr>
<tr><td valign=\"top\">EnthalpyFlowRate</td><td valign=\"top\"><code>Real (final quantity=\"EnthalpyFlowRate\", final unit=\"W\")</code></td></tr>
<tr><td colspan=\"2\"><b>Modelica.</b></td></tr>
<tr><td valign=\"top\"><b><i>Thermal.HeatTransfer</i></b></td><td valign=\"top\">1-dimensional heat transfer with lumped elements</td></tr>
<tr><td colspan=\"2\"><b>ModelicaAdditions.Blocks.Discrete.</b></td></tr>
<tr><td valign=\"top\">TriggeredSampler</td><td valign=\"top\">Triggered sampling of continuous signals</td></tr>
<tr><td valign=\"top\">TriggeredMax</td><td valign=\"top\">Compute maximum, absolute value of continuous signal at trigger instants</td></tr>
<tr><td colspan=\"2\"><b>ModelicaAdditions.Blocks.Logical.Interfaces.</b></td></tr>
<tr><td valign=\"top\">BooleanMIRealMOs</td><td valign=\"top\">Multiple Boolean Input Multiple Real Output continuous control block with same number of inputs and outputs</td></tr>
<tr><td valign=\"top\">RealMIBooleanMOs</td><td valign=\"top\">Multiple Real Input Multiple Boolean Output continuous control block with same number of inputs and outputs</td></tr>
<tr><td colspan=\"2\"><b>ModelicaAdditions.Blocks.Logical.</b></td></tr>
<tr><td valign=\"top\">TriggeredTrapezoid</td><td valign=\"top\">Triggered trapezoid generator</td></tr>
<tr><td valign=\"top\">Hysteresis</td><td valign=\"top\">Transform Real to Boolean with Hysteresis</td></tr>
<tr><td valign=\"top\">OnOffController</td><td valign=\"top\">On-off controller</td></tr>
<tr><td valign=\"top\">Compare</td><td valign=\"top\">True, if signal of inPort1 is larger than signal of inPort2</td></tr>
<tr><td valign=\"top\">ZeroCrossing</td><td valign=\"top\">Trigger zero crossing of input signal</td></tr>
<tr><td colspan=\"2\"><b>ModelicaAdditions.</b></td></tr>
<tr><td valign=\"top\">Blocks.Multiplexer.Extractor</td><td valign=\"top\">Extract scalar signal out of signal vector dependent on IntegerRealInput index</td></tr>
<tr><td valign=\"top\">Tables.CombiTable1Ds</td><td valign=\"top\">Table look-up in one dimension (matrix/file) with only single input</td></tr>
</table>
<p>
<b>Package-specific Changes</b>
</p>
<ul>
<li>All example models made <b>encapsulated</b></li>
<li>Upper case constants changed to lower case (cf. Modelica.Constants)</li>
<li>Introduced Modelica.SIunits.Wavelength due to typo \"Wavelenght\"</li>
<li>Introduced ModelicaAdditions.Blocks.Logical.Interfaces.Comparison due to typo \"Comparision\"</li>
<li>Changed these components of *.Blocks to <code>block</code> class, which have not been already of block type</li>
<li>Changed *.Interfaces.RelativeSensor to <code>partial</code> models</li>
</ul>
<p>
<b>Class-specific Changes</b>
</p>
<p>
<i>Modelica.SIunits</i>
</p>
<p>Removed <code>final</code> from quantity attribute for <i>Mass</i> and <i>MassFlowRate</i>.</p>
<p>
<i>Modelica.Blocks.Math.Sum</i>
</p>
<p>Implemented avoiding algorithm section, which would lead to expensive function calls.</p>
<p><i>Modelica.Blocks.Sources.Step</i></p>
<pre>
block Step \"Generate step signals of type Real\"
        parameter Real height[:]={1} \"Heights of steps\";
<b> // parameter Real offset[:]={0} \"Offsets of output signals\";
// parameter SIunits.Time startTime[:]={0} \"Output = offset for time < startTime\";
// extends Interfaces.MO          (final nout=max([size(height, 1); size(offset, 1); size(startTime, 1)]));
        extends Interfaces.SignalSource(final nout=max([size(height, 1); size(offset, 1); size(startTime, 1)]));</b>
</pre>
<p><i>Modelica.Blocks.Sources.Exponentials</i></p>
<p>Replaced usage of built-in function <code>exp</code> by Modelica.Math.exp.</p>
<p><i>Modelica.Blocks.Sources.TimeTable</i></p>
<p>Interface definition changed from</p>
<pre>    parameter Real table[:, :]=[0, 0; 1, 1; 2, 4] \"Table matrix (time = first column)\";
</pre>
<p>to</p>
<pre>    parameter Real table[:, <b>2</b>]=[0, 0; 1, 1; 2, 4] \"Table matrix (time = first column)\";
</pre>
<p>Did the same for subfunction <i>getInterpolationCoefficients</i>.</p>
<p>Bug in <i>getInterpolationCoefficients</i> for startTime <> 0 fixed:</p>
<pre>        ...
                end if;
          end if;
          <b>// Take into account startTime \"a*(time - startTime) + b\"
          b := b - a*startTime;</b>
        end getInterpolationCoefficients;
</pre>
<p><i>Modelica.Blocks.Sources.BooleanStep</i></p>
<pre>
block BooleanStep \"Generate step signals of type Boolean\"
        parameter SIunits.Time startTime[:]={0} \"Time instants of steps\";
        <b>parameter Boolean startValue[size(startTime, 1)]=fill(false, size(startTime, 1)) \"Output before startTime\";</b>
        extends Interfaces.BooleanSignalSource(final nout=size(startTime, 1));
equation
        for i in 1:nout loop
<b>//   outPort.signal[i] = time >= startTime[i];
          outPort.signal[i] = if time >= startTime[i] then not startValue[i] else startValue[i];</b>
        end for;
end BooleanStep;
</pre>
<p>
<i>Modelica.Electrical.Analog</i></p>
<p>Corrected table of values and default for Beta by dividing them by 1000
(consistent with the values used in the NAND-example model):
</p>
<ul>
<li>Semiconductors.PMOS</li>
<li>Semiconductors.NMOS</li>
</ul>
<p>Corrected parameter defaults, unit and description for TrapezoidCurrent.
This makes the parameters consistent with their use in the model.
Models specifying parameter values are not changed.
Models not specifying parameter values did not generate trapezoids previously.
</p>
<p>Icon layer background changed from transparent to white:</p>
<ul>
<li>Basic.Gyrator</li>
<li>Basic.EMF</li>
<li>Ideal.Idle</li>
<li>Ideal.Short</li>
</ul>
<p>Basic.Transformer: Replaced invalid escape characters '\\ ' and '\\[newline]' in documentation by '|'.</p>
<p><i>Modelica.Mechanics.Rotational</i></p>
<p>Removed arrows and names documentation from flanges in diagram layer</p>
<p><i>Modelica.Mechanics.Rotational.Interfaces.FrictionBase</i></p>
<p><i>Modelica.Mechanics.Rotational.Position</i></p>
<p>Replaced <code>reinit</code> by <code>initial equation</code></p>
<p><i>Modelica.Mechanics.Rotational.RelativeStates</i></p>
<p>Bug corrected by using modifier <code>stateSelect = StateSelect.prefer</code> as implementation</p>
<p><i>Modelica.Mechanics.Translational.Interfaces.flange_b</i></p>
<p>Attribute <b>fillColor=7</b> added to Rectangle on Icon layer, i.e., it is now
filled with white and not transparent any more.</p>
<p><i>Modelica.Mechanics.Translational.Position</i></p>
<p>Replaced <code>reinit</code> by <code>initial equation</code></p>
<p><i>Modelica.Mechanics.Translational.RelativeStates</i></p>
<p>Bug corrected by using modifier <code>stateSelect = StateSelect.prefer</code> as implementation</p>
<p><i>Modelica.Mechanics.Translational.Stop</i></p>
<p>Use <code>stateSelect = StateSelect.prefer</code>.</p>
<p><i>Modelica.Mechanics.Translational.Examples.PreLoad</i></p>
<p>Improved documentation and coordinate system used for example.</p>
<p><i>ModelicaAdditions.Blocks.Nonlinear.PadeDelay</i></p>
<p>Replaced <code>reinit</code> by <code>initial equation</code></p>
<p><i>ModelicaAdditions.HeatFlow1D.Interfaces</i></p>
<p>Definition of connectors <i>Surface_a</i> and <i>Surface_b</i>:<br>
<code>flow SIunits.HeatFlux q;</code> changed to <code>flow SIunits.HeatFlowRate q;</code></p>
<p><i>MultiBody.Parts.InertialSystem</i></p>
<p>Icon corrected.</p>
</html>"));
end Version_1_5;

class Version_1_4 "Version 1.4 (June 28, 2001)"
  extends Modelica.Icons.ReleaseNotes;

annotation (Documentation(info="<html>

<ul>
<li>Several minor bugs fixed.</li>
<li>New models:<br>
        Modelica.Blocks.Interfaces.IntegerRealInput/IntegerRealOutput,<br>
        Modelica.Blocks.Math.TwoInputs/TwoOutputs<br>
        Modelica.Electrical.Analog.Ideal.IdealOpAmp3Pin,<br>
        Modelica.Mechanics.Rotational.Move,<br>
        Modelica.Mechanics.Translational.Move.<br>
        </li>
</ul>
<hr>
<h4>Version 1.4.1beta1 (February 12, 2001)</h4>
<p> Adapted to Modelica 1.4</p>
<hr>
<h4>Version 1.3.2beta2 (June 20, 2000)</h4>
<ul>
        <li>New subpackage Modelica.Mechanics.<b>Translational</b></li>
        <li>Changes to Modelica.Mechanics.<b>Rotational</b>:<br>
           New elements:
<pre>
IdealGearR2T    Ideal gear transforming rotational in translational motion.
Position        Forced movement of a flange with a reference angle
                                   given as input signal
RelativeStates  Definition of relative state variables
</pre>
</li>
        <li>Changes to Modelica.<b>SIunits</b>:<br>
          Introduced new types:<br>
          type Temperature = ThermodynamicTemperature;<br>
          types DerDensityByEnthalpy, DerDensityByPressure,
          DerDensityByTemperature, DerEnthalpyByPressure,
          DerEnergyByDensity, DerEnergyByPressure<br>
          Attribute \"final\" removed from min and max values
          in order that these values can still be changed to narrow
          the allowed range of values.<br>
          Quantity=\"Stress\" removed from type \"Stress\", in order
          that a type \"Stress\" can be connected to a type \"Pressure\".</li>
        <li>Changes to Modelica.<b>Icons</b>:<br>
           New icons for motors and gearboxes.</li>
        <li>Changes to Modelica.<b>Blocks.Interfaces</b>:<br>
           Introduced a replaceable signal type into
           Blocks.Interfaces.RealInput/RealOutput:
<pre>
replaceable type SignalType = Real
</pre>
           in order that the type of the signal of an input/output block
           can be changed to a physical type, for example:
<pre>
Sine sin1(outPort(redeclare type SignalType=Modelica.SIunits.Torque))
</pre>
</li>
</ul>
<hr>
<h4>Version 1.3.1 (Dec. 13, 1999)</h4>
<p>
First official release of the library.
</p>
</html>"));
end Version_1_4;
 annotation (Documentation(info="<html>

<p>
This section summarizes the changes that have been performed
on the Modelica standard library. Furthermore, it is explained in
<a href=\"modelica://Modelica.UsersGuide.ReleaseNotes.VersionManagement\">Modelica.UsersGuide.ReleaseNotes.VersionManagement</a>
how the versions are managed with the subversion management systems.
This is especially important for maintenance (bug-fix) releases where the
main version number is not changed.
</p>

</html>"));
end ReleaseNotes;

class Contact "Contact"
  extends Modelica.Icons.Contact;

 annotation (Documentation(info="<html>
<dl><dt>The Modelica Standard Library (this Modelica package) is developed by many people from different organizations (see list below). It is licensed under the <a href=\"modelica://Modelica.UsersGuide.ModelicaLicense2\">Modelica License 2</a> by:</dt>
<dt>&nbsp;</dt>
<dd>Modelica Association</dd>
<dd>(Ideella F&ouml;reningar 822003-8858 in Link&ouml;ping)</dd>
<dd>c/o PELAB, IDA, Link&ouml;pings Universitet</dd>
<dd>S-58183 Link&ouml;ping</dd>
<dd>Sweden</dd>
<dd>email: <a href=\"mailto:Board@Modelica.org\">Board@Modelica.org</a></dd>
<dd>web: <a href=\"https://www.Modelica.org\">https://www.Modelica.org</a></dd>
<dd>&nbsp;&nbsp;</dd>

<dt>The development of this Modelica package, starting with version 3.2.1, is organized by:</dt>
<dd><a href=\"http://www.haumer.at/eindex.htm\">Anton Haumer</a></dd>
<dd>Technical Consulting &amp; Electrical Engineering</dd>
<dd>A-3423 St.Andrae-Woerdern, Hadersfelderweg 21</dd>
<dd>Austria</dd>
<dd>email: <a href=\"mailto:A.Haumer@Haumer.at\">A.Haumer@Haumer.at</a></dd>
<dd>&nbsp;&nbsp;</dd>

<dt>The development of this Modelica package up to and including version 3.2.1 was organized by:</dt>
<dd><a href=\"http://www.robotic.dlr.de/Martin.Otter/\">Martin Otter</a></dd>
<dd>German Aerospace Center (DLR)</dd>
<dd>Robotics and Mechatronics Center (RMC)</dd>
<dd>Institute of System Dynamics and Control (SR)</dd>
<dd>Postfach 1116</dd>
<dd>D-82230 Wessling</dd>
<dd>Germany</dd>
<dd>email: <a href=\"mailto:Martin.Otter@dlr.de\">Martin.Otter@dlr.de</a></dd>
</dl>
<p>Since end of 2007, the development of the sublibraries of package Modelica is organized by personal and/or organizational <b>library officers</b> assigned by the Modelica Association. They are responsible for the maintenance and for the further organization of the development. Other persons may also contribute, but the final decision for library improvements and/or changes is performed by the responsible library officer(s). In order that a new sublibrary or a new version of a sublibrary is ready to be released, the responsible library officers report the changes to the members of the Modelica Association and the library is made available for beta testing to interested parties before a final decision. A new release of a sublibrary is formally decided by voting of the Modelica Association members.</p>
<p>The following library officers are currently assigned:</p>

<table border=1 cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><b>Sublibraries</b> </td>
   <td valign=\"top\"><b>Library officers</b></td>
</tr>

<tr><td valign=\"top\"> Complex </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
        (Martin Otter)<br>
        Anton Haumer, Consultant, St.Andrae-Woerdern, Austria, <br>
        Christian Kral, Vienna, Austria, <br>
        AIT, Vienna, Austria
</td>

</tr>

<tr><td valign=\"top\"> Modelica.Blocks <br>
                      Modelica.Constants </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
        (Martin Otter)</td>
</tr>

<tr><td valign=\"top\"> Modelica.Electrical.Analog<br>
                      Modelica.Electrical.Digital<br>
                      Modelica.Electrical.Spice3 </td>
   <td valign=\"top\"> Fraunhofer Institute for Integrated Circuits, Dresden, Germany<br>
      (Christoph Clauss)</td>
</tr>

<tr><td valign=\"top\"> Modelica.ComplexBlocks<br>
                      Modelica.Electrical.Machines<br>
                      Modelica.Electrical.MultiPhase<br>
                      Modelica.Electrical.QuasiStationary </td>
   <td valign=\"top\"> Anton Haumer, Consultant, St.Andrae-Woerdern, Austria,<br>
        Christian Kral, Vienna, Austria, <br>
        AIT, Vienna, Austria</td>
</tr>

<tr><td valign=\"top\"> Modelica.Magnetic.FluxTubes </td>
   <td valign=\"top\"> Thomas B&ouml;drich, Dresden, Germany<br>
                               (Dresden University of Technology,<br>
                               Institute of Electromechanical and Electronic Design)
</td>
</tr>

<tr><td valign=\"top\"> Modelica.Magnetic.FundamentalWave </td>
   <td valign=\"top\"> Christian Kral, Vienna, Austria, <br>
                     AIT, Vienna, Austria<br>
                     Anton Haumer, Consultant, St.Andrae-Woerdern, Austria
</td>
</tr>

<tr><td valign=\"top\"> Modelica.Fluid </td>
   <td valign=\"top\"> Politecnico di Milano (Francesco Casella), and<br>
                            R&uuml;diger Franke (ABB)</td>
</tr>

<tr><td valign=\"top\"> Modelica.Fluid.Dissipation </td>
   <td valign=\"top\"> XRG Simulation, Hamburg, Germany (Stefan Wischhusen)</td>
</tr>

<tr><td valign=\"top\"> Modelica.Icons </td>
   <td valign=\"top\"> Modelon AB, Lund, Sweden (Johan Andreasson) </td>
</tr>

<tr><td valign=\"top\"> Modelica.Math </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter)</td>
</tr>

<tr><td valign=\"top\"> Modelica.ComplexMath </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter)<br>
      Anton Haumer, Consultant, St.Andrae-Woerdern, Austria,<br>
      Christian Kral, Vienna, Austria, <br>
      AIT, Vienna, Austria
   </td>
</tr>

<tr><td valign=\"top\"> Modelica.Mechanics.MultiBody </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter),<br>
       Modelon AB, Lund, Sweden (Johan Andreasson) </td>
</tr>

<tr><td valign=\"top\"> Modelica.Mechanics.Rotational </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter)<br>
      Anton Haumer, Consultant, St.Andrae-Woerdern, Austria,<br>
      Christian Kral, Vienna, Austria, <br>
      AIT, Vienna, Austria, <br>
      Modelon AB, Lund, Sweden (Johan Andreasson)</td>
</tr>

<tr><td valign=\"top\"> Modelica.Mechanics.Translational </td>
   <td valign=\"top\"> Anton Haumer, Consultant, St.Andrae-Woerdern, Austria,<br>
      Christian Kral, Vienna, Austria, <br>
      AIT, Vienna, Austria,<br>
      DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter)<br>
       Modelon AB, Lund, Sweden (Johan Andreasson)</td>
</tr>

<tr><td valign=\"top\"> Modelica.Media </td>
   <td valign=\"top\"> Modelon AB, Lund, Sweden (Hubertus Tummescheit) </td>
</tr>

<tr><td valign=\"top\"> Modelica.SIunits <br>
      Modelica.StateGraph </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter)</td>
</tr>

<tr><td valign=\"top\"> Modelica.Thermal.FluidHeatFlow <br>
      Modelica.Thermal.HeatTransfer </td>
   <td valign=\"top\"> Anton Haumer, Consultant, St.Andrae-Woerdern, Austria, and<br>
      Christian Kral, Vienna, Austria, <br>
      AIT, Vienna, Austria</td></tr>

<tr><td valign=\"top\"> Modelica.Utilities </td>
   <td valign=\"top\"> DLR Institute of System Dynamics and Control, Oberpfaffenhofen, Germany<br>
      (Martin Otter)<br>
      Dassault Syst&egrave;mes AB, Lund, Sweden (Hans Olsson)</td>
</tr>
</table>

<p>
The following people have directly contributed to the implementation
of the Modelica package (many more people have contributed to the design):
</p>

<table border=1 cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><b>Marcus Baur</b> </td>
   <td valign=\"top\"> Institute of System Dynamics and Control<br>
     DLR, German Aerospace Center, <br>
     Oberpfaffenhofen, Germany</td>
   <td valign=\"top\"> Complex<br>
                     Modelica.Math.Vectors<br>
                     Modelica.Math.Matrices</td>
</tr>

<tr><td valign=\"top\"><b>Peter Beater</b> </td>
   <td valign=\"top\"> University of Paderborn, Germany</td>
   <td valign=\"top\"> Modelica.Mechanics.Translational </td>
</tr>



<tr><td valign=\"top\"><b>Thomas Beutlich</b> </td>
   <td valign=\"top\"> ITI GmbH, Germany</td>
   <td valign=\"top\"> Modelica.Blocks.Sources.CombiTimeTable<br>
                       Modelica.Blocks.Tables </td>
</tr>

<tr><td valign=\"top\"><b>Thomas B&ouml;drich</b> </td>
   <td valign=\"top\"> Dresden University of Technology, Germany</td>
   <td valign=\"top\"> Modelica.Magnetic.FluxTubes </td>
</tr>

<tr><td valign=\"top\"><b>Dag Br&uuml;ck</b> </td>
   <td valign=\"top\"> Dassault Syst&egrave;mes AB, Lund, Sweden</td>
   <td valign=\"top\"> Modelica.Utilities</td>
</tr>

<tr><td valign=\"top\"><b>Francesco Casella</b> </td>
   <td valign=\"top\"> Politecnico di Milano, Milano, Italy</td>
   <td valign=\"top\"> Modelica.Fluid<br>
                            Modelica.Media</td>
</tr>

<tr><td valign=\"top\"><b>Christoph Clauss</b> </td>
   <td valign=\"top\"> Fraunhofer Institute for Integrated Circuits,<br> Dresden, Germany</td>
   <td valign=\"top\"> Modelica.Electrical.Analog<br>
     Modelica.Electrical.Digital<br>
     Modelica.Electrical.Spice3</td>
</tr>

<tr><td valign=\"top\"><b>Jonas Eborn</b> </td>
   <td valign=\"top\"> Modelon AB, Lund, Sweden</td>
   <td valign=\"top\"> Modelica.Media</td>
</tr>

<tr><td valign=\"top\"><b>Hilding Elmqvist</b> </td>
   <td valign=\"top\"> Dassault Syst&egrave;mes AB, Lund, Sweden</td>
   <td valign=\"top\"> Modelica.Mechanics.MultiBody<br>
                   Modelica.Fluid<br>
     Modelica.Media<br>
     Modelica.StateGraph<br>
     Modelica.Utilities<br>
     Conversion from 1.6 to 2.0</td>
</tr>

<tr><td valign=\"top\"><b>R&uuml;diger Franke</b> </td>
   <td valign=\"top\"> ABB Corporate Research,<br>Ladenburg, German</td>
   <td valign=\"top\"> Modelica.Fluid<br>
                            Modelica.Media</td>
</tr>

<tr><td valign=\"top\"><b>Manuel Gr&auml;ber</b> </td>
   <td valign=\"top\"> Institut f&uuml;r Thermodynamik, <br>
     Technische Universit&auml;t Braunschweig, <br>
     Germany</td>
   <td valign=\"top\"> Modelica.Fluid</td>
</tr>

<tr><td valign=\"top\"><b>Anton Haumer</b> </td>
   <td valign=\"top\"> Consultant, St.Andrae-Woerdern,<br>Austria</td>
   <td valign=\"top\"> Modelica.ComplexBlocks<br>
     Modelica.Electrical.Machines<br>
     Modelica.Electrical.Multiphase<br>
     Modelica.Electrical.QuasiStationary<br>
     Modelica.Magnetics.FundamentalWave<br>
     Modelica.Mechanics.Rotational<br>
     Modelica.Mechanics.Translational<br>
     Modelica.Thermal.FluidHeatFlow<br>
     Modelica.Thermal.HeatTransfer<br>
     Modelica.ComplexMath<br>
     Conversion from 1.6 to 2.0<br>
     Conversion from 2.2 to 3.0</td>
</tr>

<tr><td valign=\"top\"><b>Hans-Dieter Joos</b> </td>
   <td valign=\"top\"> Institute of System Dynamics and Control<br>
     DLR, German Aerospace Center, <br>
     Oberpfaffenhofen, Germany</td>
   <td valign=\"top\"> Modelica.Math.Matrices</td>
</tr>

<tr><td valign=\"top\"><b>Christian Kral</b> </td>
   <td valign=\"top\"> Modeling and Simulation of Electric Machines, Drives and Mechatronic Systems, Vienna, Austria</td>
   <td valign=\"top\"> Modelica.ComplexBlocks<br>
     Modelica.Electrical.Machines<br>
     Modelica.Electrical.MultiPhase<br>
     Modelica.Electrical.QuasiStationary<br>
     Modelica.Magnetics.FundamentalWave<br>
     Modelica.Mechanics.Rotational<br>
     Modelica.Mechanics.Translational<br>
     Modelica.Thermal.FluidHeatFlow<br>
     Modelica.Thermal.HeatTransfer<br>
     Modelica.ComplexMath
     </td>
</tr>

<tr><td valign=\"top\"><b>Sven Erik Mattsson</b> </td>
   <td valign=\"top\"> Dassault Syst&egrave;mes AB, Lund, Sweden</td>
   <td valign=\"top\"> Modelica.Mechanics.MultiBody</td>
</tr>
<tr><td valign=\"top\"><b>Hans Olsson</b> </td>
   <td valign=\"top\"> Dassault Syst&egrave;mes AB, Lund, Sweden</td>
   <td valign=\"top\"> Modelica.Blocks<br>
     Modelica.Math.Matrices<br>
     Modelica.Utilities<br>
     Conversion from 1.6 to 2.0<br>
     Conversion from 2.2 to 3.0</td>
</tr>

<tr><td valign=\"top\"><b>Martin Otter</b> </td>
   <td valign=\"top\"> Institute of System Dynamics and Control<br>
     DLR, German Aerospace Center, <br>
     Oberpfaffenhofen, Germany</td>
   <td valign=\"top\"> Complex<br>
     Modelica.Blocks<br>
     Modelica.Fluid<br>
     Modelica.Mechanics.MultiBody<br>
     Modelica.Mechanics.Rotational<br>
     Modelica.Mechanics.Translational<br>
     Modelica.Math<br>
     Modelica.ComplexMath<br>
     Modelica.Media<br>
     Modelica.SIunits<br>
     Modelica.StateGraph<br>
     Modelica.Thermal.HeatTransfer<br>
     Modelica.Utilities<br>
     ModelicaReference<br>
     Conversion from 1.6 to 2.0<br>
     Conversion from 2.2 to 3.0</td>
</tr>

<tr><td valign=\"top\"><b>Katrin Pr&ouml;l&szlig;</b> </td>
   <td valign=\"top\"> Modelon AB, Lund, Sweden<br>
                            until 2008:<br>
                            Department of Technical Thermodynamics,<br>
     Technical University Hamburg-Harburg,<br>Germany</td>
   <td valign=\"top\"> Modelica.Fluid<br>
                            Modelica.Media</td>
</tr>

<tr><td valign=\"top\"><b>Christoph C. Richter</b> </td>
   <td valign=\"top\"> until 2009:<br>
     Institut f&uuml;r Thermodynamik, <br>
     Technische Universit&auml;t Braunschweig, <br>
     Germany</td>
   <td valign=\"top\"> Modelica.Fluid<br>
                            Modelica.Media</td>
</tr>

<tr><td valign=\"top\"><b>Andr&eacute; Schneider</b> </td>
   <td valign=\"top\"> Fraunhofer Institute for Integrated Circuits,<br> Dresden, Germany</td>
   <td valign=\"top\"> Modelica.Electrical.Analog<br>
     Modelica.Electrical.Digital</td>
</tr>
<tr><td valign=\"top\"><b>Christian Schweiger</b> </td>
   <td valign=\"top\"> Until 2006:<br>
     Institute of System Dynamics and Control,<br>
     DLR, German Aerospace Center,<br>
     Oberpfaffenhofen, Germany</td>
   <td valign=\"top\"> Modelica.Mechanics.Rotational<br>
     ModelicaReference<br>
     Conversion from 1.6 to 2.0</td>
</tr>

<tr><td valign=\"top\"><b>Michael Sielemann</b> </td>
   <td valign=\"top\"> Institute of System Dynamics and Control<br>
     DLR, German Aerospace Center, <br>
     Oberpfaffenhofen, Germany</td>
   <td valign=\"top\"> Modelica.Fluid<br>
                       Modelica.Media</td>
</tr>

<tr><td valign=\"top\"><b>Michael Tiller</b> </td>
   <td valign=\"top\"> Emmeskay, Inc., Dearborn, MI, U.S.A, <br>
     (previously Ford Motor Company, Dearborn) </td>
   <td valign=\"top\"> Modelica.Media<br>
     Modelica.Thermal.HeatTransfer</td>
</tr>
<tr><td valign=\"top\"><b>Hubertus Tummescheit</b> </td>
   <td valign=\"top\"> Modelon AB, Lund, Sweden </td>
   <td valign=\"top\"> Modelica.Media<br>
     Modelica.Thermal.HeatTransfer</td>
</tr>

<tr><td valign=\"top\"><b>Thorsten Vahlenkamp</b> </td>
   <td valign=\"top\"> until 2010:<br>
                     XRG Simulation GmbH, Hamburg, Germany</td>
   <td valign=\"top\"> Modelica.Fluid.Dissipation</td>
</tr>

<tr><td valign=\"top\"><b>Nico Walter</b> </td>
   <td valign=\"top\"> Master thesis at HTWK Leipzig<br>
     (Prof. R. M&uuml;ller) and<br>
     DLR Oberpfaffenhofen, Germany</td>
   <td valign=\"top\"> Modelica.Math.Matrices</td>
</tr>

<tr><td valign=\"top\"><b>Michael Wetter</b> </td>
   <td valign=\"top\"> Lawrence Berkeley National Laboratory; U.S.A.</td>
   <td valign=\"top\"> Modelica.Fluid</td>
</tr>

<tr><td valign=\"top\"><b>Hans-J&uuml;rg Wiesmann</b> </td>
   <td valign=\"top\"> Switzerland</td>
   <td valign=\"top\"> Modelica.ComplexMath</td>
</tr>

<tr><td valign=\"top\"><b>Stefan Wischhusen</b> </td>
   <td valign=\"top\"> XRG Simulation GmbH, Hamburg, Germany</td>
   <td valign=\"top\"> Modelica.Fluid.Dissipation<br>
                       Modelica.Media</td>
</tr>
</table>

</html>"));

end Contact;

annotation (DocumentationClass=true, Documentation(info="<html>
<p>
Package <b>Modelica</b> is a <b>standardized</b> and <b>pre-defined</b> package
that is developed together with the Modelica language from the
Modelica Association, see
<a href=\"https://www.Modelica.org\">https://www.Modelica.org</a>.
It is also called <b>Modelica Standard Library</b>.
It provides constants, types, connectors, partial models and model
components in various disciplines.
</p>
<p>
This is a short <b>User's Guide</b> for
the overall library. Some of the main sublibraries have their own
User's Guides that can be accessed by the following links:
</p>

<table border=1 cellspacing=0 cellpadding=2>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Electrical.Digital.UsersGuide\">Digital</a>
   </td>
   <td valign=\"top\">Library for digital electrical components based on the VHDL standard
   (2-,3-,4-,9-valued logic)</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Magnetic.FluxTubes.UsersGuide\">FluxTubes</a>
    </td>
   <td valign=\"top\">Library for modelling of electromagnetic devices with lumped magnetic networks</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.MultiBody.UsersGuide\">MultiBody</a>
    </td>
   <td valign=\"top\">Library to model 3-dimensional mechanical systems</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.Rotational.UsersGuide\">Rotational</a>
    </td>
   <td valign=\"top\">Library to model 1-dimensional, rotational mechanical systems</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Mechanics.Translational.UsersGuide\">Translational</a>
    </td>
   <td valign=\"top\">Library to model 1-dimensional, translational mechanical systems</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Fluid.UsersGuide\">Fluid</a></td>
    <td valign=\"top\">Library of 1-dim. thermo-fluid flow models using the Modelica.Media media description</td>
</tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.Media.UsersGuide\">Media</a>
    </td>
   <td valign=\"top\">Library of media property models</td>
</tr>
<tr><td valign=\"top\"><a href=\"modelica://Modelica.SIunits.UsersGuide\">SIunits</a> </td>
   <td valign=\"top\">Library of type definitions based on SI units according to ISO 31-1992</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.StateGraph.UsersGuide\">StateGraph</a>
    </td>
   <td valign=\"top\">Library to model discrete event and reactive systems by hierarchical state machines</td>
</tr>

<tr><td valign=\"top\"><a href=\"modelica://Modelica.Utilities.UsersGuide\">Utilities</a>
    </td>
   <td valign=\"top\">Library of utility functions especially for scripting (Files, Streams, Strings, System)</td>
</tr>
</table>

</html>"));
end UsersGuide;


annotation (
preferredView="info",
version="3.2.2",
versionBuild=3,
versionDate="2016-04-03",
dateModified = "2016-04-03 08:44:41Z",
revisionId="$Id::                                       $",
uses(Complex(version="3.2.2"), ModelicaServices(version="3.2.2")),
conversion(
 noneFromVersion="3.2.1",
 noneFromVersion="3.2",
 noneFromVersion="3.1",
 noneFromVersion="3.0.1",
 noneFromVersion="3.0",
 from(version="2.1", script="modelica://Modelica/Resources/Scripts/Dymola/ConvertModelica_from_2.2.2_to_3.0.mos"),
 from(version="2.2", script="modelica://Modelica/Resources/Scripts/Dymola/ConvertModelica_from_2.2.2_to_3.0.mos"),
 from(version="2.2.1", script="modelica://Modelica/Resources/Scripts/Dymola/ConvertModelica_from_2.2.2_to_3.0.mos"),
 from(version="2.2.2", script="modelica://Modelica/Resources/Scripts/Dymola/ConvertModelica_from_2.2.2_to_3.0.mos")),
Icon(coordinateSystem(extent={{-100.0,-100.0},{100.0,100.0}}), graphics={
  Polygon(
    origin={-6.9888,20.048},
    fillColor={0,0,0},
    pattern=LinePattern.None,
    fillPattern=FillPattern.Solid,
    points={{-93.0112,10.3188},{-93.0112,10.3188},{-73.011,24.6},{-63.011,31.221},{-51.219,36.777},{-39.842,38.629},{-31.376,36.248},{-25.819,29.369},{-24.232,22.49},{-23.703,17.463},{-15.501,25.135},{-6.24,32.015},{3.02,36.777},{15.191,39.423},{27.097,37.306},{32.653,29.633},{35.035,20.108},{43.501,28.046},{54.085,35.19},{65.991,39.952},{77.897,39.688},{87.422,33.338},{91.126,21.696},{90.068,9.525},{86.099,-1.058},{79.749,-10.054},{71.283,-21.431},{62.816,-33.337},{60.964,-32.808},{70.489,-16.14},{77.368,-2.381},{81.072,10.054},{79.749,19.05},{72.605,24.342},{61.758,23.019},{49.587,14.817},{39.003,4.763},{29.214,-6.085},{21.012,-16.669},{13.339,-26.458},{5.401,-36.777},{-1.213,-46.037},{-6.24,-53.446},{-8.092,-52.387},{-0.684,-40.746},{5.401,-30.692},{12.81,-17.198},{19.424,-3.969},{23.658,7.938},{22.335,18.785},{16.514,23.283},{8.047,23.019},{-1.478,19.05},{-11.267,11.113},{-19.734,2.381},{-29.259,-8.202},{-38.519,-19.579},{-48.044,-31.221},{-56.511,-43.392},{-64.449,-55.298},{-72.386,-66.939},{-77.678,-74.612},{-79.53,-74.083},{-71.857,-61.383},{-62.861,-46.037},{-52.278,-28.046},{-44.869,-15.346},{-38.784,-2.117},{-35.344,8.731},{-36.403,19.844},{-42.488,23.813},{-52.013,22.49},{-60.744,16.933},{-68.947,10.054},{-76.884,2.646},{-93.0112,-12.1707},{-93.0112,-12.1707}},
    smooth=Smooth.Bezier),
  Ellipse(
    origin={40.8208,-37.7602},
    fillColor={161,0,4},
    pattern=LinePattern.None,
    fillPattern=FillPattern.Solid,
    extent={{-17.8562,-17.8563},{17.8563,17.8562}})}),
Documentation(info="<html>
<p>
Package <b>Modelica&reg;</b> is a <b>standardized</b> and <b>free</b> package
that is developed together with the Modelica&reg; language from the
Modelica Association, see
<a href=\"https://www.Modelica.org\">https://www.Modelica.org</a>.
It is also called <b>Modelica Standard Library</b>.
It provides model components in many domains that are based on
standardized interface definitions. Some typical examples are shown
in the next figure:
</p>

<p>
<img src=\"modelica://Modelica/Resources/Images/UsersGuide/ModelicaLibraries.png\">
</p>

<p>
For an introduction, have especially a look at:
</p>
<ul>
<li> <a href=\"modelica://Modelica.UsersGuide.Overview\">Overview</a>
  provides an overview of the Modelica Standard Library
  inside the <a href=\"modelica://Modelica.UsersGuide\">User's Guide</a>.</li>
<li><a href=\"modelica://Modelica.UsersGuide.ReleaseNotes\">Release Notes</a>
 summarizes the changes of new versions of this package.</li>
<li> <a href=\"modelica://Modelica.UsersGuide.Contact\">Contact</a>
  lists the contributors of the Modelica Standard Library.</li>
<li> The <b>Examples</b> packages in the various libraries, demonstrate
  how to use the components of the corresponding sublibrary.</li>
</ul>

<p>
This version of the Modelica Standard Library consists of
</p>
<ul>
<li><b>1600</b> models and blocks, and</li>
<li><b>1350</b> functions</li>
</ul>
<p>
that are directly usable (= number of public, non-partial classes). It is fully compliant
to <a href=\"https://www.modelica.org/documents/ModelicaSpec32Revision2.pdf\">Modelica Specification Version 3.2 Revision 2</a>
and it has been tested with Modelica tools from different vendors.
</p>

<p>
<b>Licensed by the Modelica Association under the Modelica License 2</b><br>
Copyright &copy; 1998-2016, ABB, AIT, T.&nbsp;B&ouml;drich, DLR, Dassault Syst&egrave;mes AB, Fraunhofer, A.&nbsp;Haumer, ITI, C.&nbsp;Kral, Modelon,
TU Hamburg-Harburg, Politecnico di Milano, XRG Simulation.
</p>

<p>
<i>This Modelica package is <u>free</u> software and the use is completely at <u>your own risk</u>; it can be redistributed and/or modified under the terms of the Modelica License 2. For license conditions (including the disclaimer of warranty) see <a href=\"modelica://Modelica.UsersGuide.ModelicaLicense2\">Modelica.UsersGuide.ModelicaLicense2</a> or visit <a href=\"https://www.modelica.org/licenses/ModelicaLicense2\"> https://www.modelica.org/licenses/ModelicaLicense2</a>.</i>
</p>

<p>
<b>Modelica&reg;</b> is a registered trademark of the Modelica Association.
</p>
</html>"));
end Modelica;
