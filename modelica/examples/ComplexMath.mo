within Modelica;
package ComplexMath
  "Library of complex mathematical functions (e.g., sin, cos) and of functions operating on complex vectors and matrices"
  extends Modelica.Icons.Package;
  final constant Complex j = Complex(0,1) "Imaginary unit";

package Vectors "Library of functions operating on complex vectors"
  extends Modelica.Icons.Package;

function norm "Returns the p-norm of a complex vector"
  extends Modelica.Icons.Function;
  input Complex v[:] "Vector";
  input Real p(min=1) = 2
        "Type of p-norm (often used: 1, 2, or Modelica.Constants.inf)";
  output Real result "p-norm of vector v";

algorithm
  if p == 2 then
    result:= sqrt(sum(v[i].re^2 + v[i].im^2 for i in 1:size(v,1)));
  elseif p == Modelica.Constants.inf then
    result:= ComplexMath.'abs'(ComplexMath.'max'(v));
  elseif p == 1 then
    result:= sum(ComplexMath.'abs'(v[i]) for i in 1:size(v,1));
  else
    result:=(sum(ComplexMath.'abs'(v[i])^p for i in 1:size(v, 1)))^(1/p);
  end if;

  annotation (Documentation(info="<html>
<h4>Syntax</h4>
<blockquote><pre>
Vectors.<b>norm</b>(v);
Vectors.<b>norm</b>(v,p=2);   // 1 &le; p &le; &#8734;
</pre></blockquote>

<h4>Description</h4>
<p>
The function call \"<code>Vectors.<b>norm</b>(v)</code>\" returns the
<b>Euclidean norm</b> \"<code>sqrt(v*v)</code>\" of vector v.
With the optional
second argument \"p\", any other p-norm can be computed:
</p>
<center>
<IMG src=\"modelica://Modelica/Resources/Images/Math/Vectors/vectorNorm.png\" ALT=\"function Vectors.norm\">
</center>
<p>
Besides the Euclidean norm (p=2), also the 1-norm and the
infinity-norm are sometimes used:
</p>
<table border=1 cellspacing=0 cellpadding=2>
  <tr><td><b>1-norm</b></td>
      <td>= sum(abs(v))</td>
      <td><b>norm</b>(v,1)</td>
  </tr>
  <tr><td><b>2-norm</b></td>
      <td>= sqrt(v*v)</td>
      <td><b>norm</b>(v) or <b>norm</b>(v,2)</td>
  </tr>
  <tr><td><b>infinity-norm</b></td>
      <td>= max(abs(v))</td>
      <td><b>norm</b>(v,Modelica.Constants.<b>inf</b>)</td>
  </tr>
</table>
<p>
Note, for any vector norm the following inequality holds:
</p>
<blockquote><pre>
<b>norm</b>(v1+v2,p) &le; <b>norm</b>(v1,p) + <b>norm</b>(v2,p)
</pre></blockquote>

<h4>Example</h4>
<blockquote><pre>
  v = {2, -4, -2, -1};
  <b>norm</b>(v,1);    // = 9
  <b>norm</b>(v,2);    // = 5
  <b>norm</b>(v);      // = 5
  <b>norm</b>(v,10.5); // = 4.00052597412635
  <b>norm</b>(v,Modelica.Constants.inf);  // = 4
</pre></blockquote>

<h4>See also</h4>
<p>
<a href=\"modelica://Modelica.Math.Matrices.norm\">Matrices.norm</a>
</p>
</html>"));
end norm;

function length "Return length of a complex vector"
  extends Modelica.Icons.Function;
  input Complex v[:] "Vector";
  output Real result "Length of vector v";

algorithm
  result := sqrt(sum({v[i].re^2 + v[i].im^2 for i in 1:size(v,1)}));
  annotation (Documentation(info="<html>
<h4>Syntax</h4>
<blockquote><pre>
Vectors.<b>length</b>(v);
</pre></blockquote>

<h4>Description</h4>

<p>
The function call \"<code>Vectors.<b>length</b>(v)</code>\" returns the
<b>Euclidean length</b> \"<code>sqrt(v*v)</code>\" of vector v.
The function call is equivalent to Vectors.norm(v). The advantage of
length(v) over norm(v)\"is that function length(..) is implemented
in one statement and therefore the function is usually automatically
inlined. Further symbolic processing is therefore possible, which is
not the case with function norm(..).
</p>

<h4>Example</h4>
<blockquote><pre>
  v = {2, -4, -2, -1};
  <b>length</b>(v);  // = 5
</pre></blockquote>

<h4>See also</h4>
<p>
<a href=\"modelica://Modelica.Math.Vectors.norm\">Vectors.norm</a>
</p>
</html>"));
end length;

function normalize
      "Return normalized complex vector such that length = 1 and prevent zero-division for zero vector"
  extends Modelica.Icons.Function;
  input Complex v[:] "Vector";
  input Real eps = 100*Modelica.Constants.eps "if |v| < eps then result = v";
  output Complex result[size(v, 1)] "Input vector v normalized to length=1";

    protected
  Real length_v = length(v);
algorithm
  if length_v >= eps then
     for i in 1:size(v,1) loop
         result[i] :=v[i].re/length_v + (v[i].im/length_v)*j;
     end for;
  else
     result :=v;
  end if;

  annotation (Documentation(info="<html>
<h4>Syntax</h4>
<blockquote><pre>
Vectors.<b>normalize</b>(v);
Vectors.<b>normalize</b>(v,eps=100*Modelica.Constants.eps);
</pre></blockquote>

<h4>Description</h4>
<p>
The function call \"<code>Vectors.<b>normalize</b>(v)</code>\" returns the
<b>unit vector</b> \"<code>v/length(v)</code>\" of vector v.
If length(v) is close to zero (more precisely, if length(v) &lt; eps),
v is returned in order to avoid
a division by zero. For many applications this is useful, because
often the unit vector <b>e</b> = <b>v</b>/length(<b>v</b>) is used to compute
a vector x*<b>e</b>, where the scalar x is in the order of length(<b>v</b>),
i.e., x*<b>e</b> is small, when length(<b>v</b>) is small and then
it is fine to replace <b>e</b> by <b>v</b> to avoid a division by zero.
</p>
<p>
Since the function is implemented in one statement,
it is usually inlined and therefore symbolic processing is
possible.
</p>

<h4>Example</h4>
<blockquote><pre>
  <b>normalize</b>({1,2,3});  // = {0.267, 0.534, 0.802}
  <b>normalize</b>({0,0,0});  // = {0,0,0}
</pre></blockquote>

<h4>See also</h4>
<p>
<a href=\"modelica://Modelica.Math.Vectors.length\">Vectors.length</a>
</p>
</html>"));
end normalize;

function reverse "Reverse vector elements (e.g., v[1] becomes last element)"
extends Modelica.Icons.Function;

  input Complex v[:] "Vector";
  output Complex result[size(v, 1)] "Elements of vector v in reversed order";

algorithm
  result := {v[end-i+1] for i in 1:size(v,1)};
annotation (Inline=true, Documentation(info="<html>
<h4>Syntax</h4>
<blockquote><pre>Vectors.<b>reverse</b>(v);</pre></blockquote>
<h4>Description</h4>
The function call &quot;<code>Vectors.<b>reverse</b>(v)</code>&quot; returns the complex vector elements in reverse order.
<h4>Example</h4>
<blockquote><pre>  <b>reverse</b>({1,2,3,4});  // = {4,3,2,1}</pre></blockquote>
</html>"));
end reverse;

function sort "Sort elements of complex vector"
  extends Modelica.Icons.Function;
  input Complex v[:] "Vector to be sorted";
  input Boolean ascending = true
        "= true if ascending order, otherwise descending order";
  input Boolean sortFrequency=true
        "= true, if sorting is first for imaginary then for real value; = false, if sorting is for absolute value";
  output Complex sorted_v[size(v,1)] = v "Sorted vector";
  output Integer indices[size(v,1)] = 1:size(v,1) "sorted_v = v[indices]";

  /* shellsort algorithm; should be improved later */
    protected
  Integer gap;
  Integer i;
  Integer j;
  Complex wv;
  Integer wi;
  Integer nv = size(v,1);
  Boolean swap;
  Integer k1;
  Integer k2;
algorithm
  gap := div(nv,2);

  while gap > 0 loop
     i := gap;
     while i < nv loop
        j := i-gap;
        if j>=0 then
           k1 := j+1;
           k2 := j + gap + 1;
           if sortFrequency then
              if ascending then
                 swap := abs(sorted_v[k1].im) >  abs(sorted_v[k2].im) or
                         abs(sorted_v[k1].im) == abs(sorted_v[k2].im) and
                         (sorted_v[k1].re  > sorted_v[k2].re or
                          sorted_v[k1].re  == sorted_v[k2].re and sorted_v[k1].im < sorted_v[k2].im);
              else
                 swap := abs(sorted_v[k1].im) <  abs(sorted_v[k2].im) or
                         abs(sorted_v[k1].im) == abs(sorted_v[k2].im) and
                         (sorted_v[k1].re  < sorted_v[k2].re or
                          sorted_v[k1].re  == sorted_v[k2].re and sorted_v[k1].im < sorted_v[k2].im);
              end if;
           else
              if ascending then
                 swap := ComplexMath.'abs'(sorted_v[k1]) > ComplexMath.'abs'(sorted_v[k2]);
              else
                 swap := ComplexMath.'abs'(sorted_v[k1]) < ComplexMath.'abs'(sorted_v[k2]);
              end if;
           end if;
        else
           swap := false;
        end if;

        while swap loop
           wv := sorted_v[j+1];
           wi := indices[j+1];
           sorted_v[j+1] := sorted_v[j+gap+1];
           sorted_v[j+gap+1] := wv;
           indices[j+1] := indices[j+gap+1];
           indices[j+gap+1] := wi;
           j := j - gap;
           if j >= 0 then
              k1 := j+1;
              k2 := j + gap + 1;
              if sortFrequency then
                 if ascending then
                    swap := abs(sorted_v[k1].im) >  abs(sorted_v[k2].im) or
                            abs(sorted_v[k1].im) == abs(sorted_v[k2].im) and
                            (sorted_v[k1].re  > sorted_v[k2].re or
                             sorted_v[k1].re  == sorted_v[k2].re and sorted_v[k1].im < sorted_v[k2].im);
                 else
                    swap := abs(sorted_v[k1].im) <  abs(sorted_v[k2].im) or
                            abs(sorted_v[k1].im) == abs(sorted_v[k2].im) and
                            (sorted_v[k1].re  < sorted_v[k2].re or
                             sorted_v[k1].re  == sorted_v[k2].re and sorted_v[k1].im < sorted_v[k2].im);
                 end if;
              else
                 if ascending then
                    swap := ComplexMath.'abs'(sorted_v[k1]) > ComplexMath.'abs'(sorted_v[k2]);
                 else
                    swap := ComplexMath.'abs'(sorted_v[k1]) < ComplexMath.'abs'(sorted_v[k2]);
                 end if;
              end if;
           else
              swap := false;
           end if;
        end while;
        i := i + 1;
     end while;
     gap := div(gap,2);
  end while;

  annotation (Documentation(info="<html>
<h4>Syntax</h4>
<blockquote><pre>
           sorted_v = Vectors.<b>sort</b>(v);
(sorted_v, indices) = Vectors.<b>sort</b>(v, ascending=true);
</pre></blockquote>

<h4>Description</h4>
<p>
Function <b>sort</b>(..) sorts a Real vector v
in ascending order and returns the result in sorted_v.
If the optional argument \"ascending\" is <b>false</b>, the vector
is sorted in descending order. In the optional second
output argument the indices of the sorted vector with respect
to the original vector are given, such that sorted_v = v[indices].
</p>

<h4>Example</h4>
<blockquote><pre>
  (v2, i2) := Vectors.sort({-1, 8, 3, 6, 2});
       -> v2 = {-1, 2, 3, 6, 8}
          i2 = {1, 5, 3, 4, 2}
</pre></blockquote>

</html>"));
end sort;

  annotation(Documentation(info="<html>
<p>
This library provides functions operating on vectors
of Complex numbers.
</p>
</html>"));
end Vectors;

  function sin "Sine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "sin(c1)";
  algorithm
     c2 := (exp(Complex(-c1.im, +c1.re)) - exp(Complex(+c1.im, -c1.re)))/Complex(0, 2);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex sine of the Complex input.</p>
</html>"));
  end sin;

  function cos "Cosine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= cos(c1)";
  algorithm
    c2 := (exp(Complex(-c1.im, +c1.re)) + exp(Complex(+c1.im, -c1.re)))/2;
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex cosine of the Complex input.</p>
</html>"));
  end cos;

  function tan "Tangent of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= tan(c1)";
  algorithm
    c2 := sin(c1)/cos(c1);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex tangent of the Complex input.</p>
</html>"));
  end tan;

  function asin "Arc-sine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "arc_sin(c1)";
  algorithm
    c2 := -j*log(j*c1 + 'sqrt'(1 - c1*c1));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the inverse Complex sine of the Complex input.</p>
</html>"));
  end asin;

  function acos "Arc-cosine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= arc_cos(c1)";
  algorithm
    c2 := -j*log(c1 + j*'sqrt'(1 - c1*c1));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the inverse Complex cosine of the Complex input.</p>
</html>"));
  end acos;

  function atan "Arc-tangent of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= arc_tan(c1)";
  algorithm
    c2 := 0.5*j*log((j + c1)/(j - c1));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the inverse Complex tangent of the Complex input.</p>
</html>"));
  end atan;

  function sinh "Hyperbolic-sine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "sinh(c1)";
  algorithm
    c2 := Complex(Math.sinh(c1.re)*Math.cos(c1.im), Math.cosh(c1.re)*Math.sin(c1.im));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex hyperbolic sine of the Complex input.</p>
</html>"));
  end sinh;

  function cosh "Hyperbolic-cosine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= cosh(c1)";
  algorithm
    c2 := Complex(Math.cosh(c1.re)*Math.cos(c1.im), Math.sinh(c1.re)*Math.sin(c1.im));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex hyperbolic cosine of the Complex input.</p>
</html>"));
  end cosh;

  function tanh "Hyperbolic-tangent of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= tanh(c1)";
  algorithm
    c2 := sinh(c1)/cosh(c1);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex hyperbolic tangent of the Complex input.</p>
</html>"));
  end tanh;

  function asinh "Area-hyperbolic-sine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "ar_sinh(c1)";
  algorithm
    c2 := log(c1 + 'sqrt'(c1*c1 + 1));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the inverse Complex hyperbolic sine of the Complex input.</p>
</html>"));
  end asinh;

  function acosh "Area-hyperbolic-cosine of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= ar_cosh(c1)";
  algorithm
    c2 := log(c1 + (c1 + 1)*'sqrt'((c1 - 1)/(c1 + 1)));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the inverse Complex hyperbolic cosine of the Complex input.</p>
</html>"));
  end acosh;

  function atanh "Area-hyperbolic-tangent of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= ar_tanh(c1)";
  algorithm
    c2 := 0.5*log((1 + c1)/(1 - c1));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the inverse Complex hyperbolic tangent of the Complex input.</p>
</html>"));
  end atanh;

  function exp "Exponential of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= exp(c1)";
  algorithm
    c2 := Complex(Math.exp(c1.re)*Math.cos(c1.im), Math.exp(c1.re)*Math.sin(c1.im));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex natural exponential of the Complex input.</p>
</html>"));
  end exp;

  function log "Logarithm of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= log(c1)";
  algorithm
    c2 := Complex(Modelica.Math.log('abs'(c1)), arg(c1));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex natural logarithm of the Complex input.</p>
</html>"));
  end log;

  function 'abs' "Absolute value of complex number"
    extends Modelica.Icons.Function;
    input Complex c "Complex number";
    output Real result "= abs(c)";
  algorithm
    result := (c.re^2 + c.im^2)^0.5; //changed from sqrt
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Real absolute of the Complex input, i.e., its length.</p>
</html>"));
  end 'abs';

  function arg "Phase angle of complex number"
    extends Modelica.Icons.Function;
    input Complex c "Complex number";
    input Modelica.SIunits.Angle phi0=0
      "Phase angle phi shall be in the range: -pi < phi-phi0 < pi";
    output Modelica.SIunits.Angle phi "= phase angle of c";
  algorithm
    phi := Modelica.Math.atan3(
        c.im,
        c.re,
        phi0);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Real argument of the Complex input, i.e., its angle.</p>
</html>"));
  end arg;

  function conj "Conjugate of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= c1.re - j*c1.im";
  algorithm
    c2 := Complex(c1.re, -c1.im);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex conjugate of the Complex input.</p>
</html>"));
  end conj;

  function real "Real part of complex number"
    extends Modelica.Icons.Function;
    input Complex c "Complex number";
    output Real r "= c.re";
  algorithm
    r := c.re;
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the real part of the Complex input.</p>
</html>"));
  end real;

  function imag "Imaginary part of complex number"
    extends Modelica.Icons.Function;
    input Complex c "Complex number";
    output Real r "= c.im";
  algorithm
    r := c.im;
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the imaginary part of the Complex input.</p>
</html>"));
  end imag;

  function fromPolar "Complex from polar representation"
    extends Modelica.Icons.Function;
    input Real len "abs of complex";
    input Modelica.SIunits.Angle phi "arg of complex";
    output Complex c "= len*cos(phi) + j*len*sin(phi)";
  algorithm
    c := Complex(len*Modelica.Math.cos(phi), len*Modelica.Math.sin(phi));
    annotation(Inline=true, Documentation(info="<html>
<p>This function constructs a Complex number from its length (absolute) and angle (argument).</p>
</html>"));
  end fromPolar;

  function 'sqrt' "Square root of complex number"
    extends Modelica.Icons.Function;
    input Complex c1 "Complex number";
    output Complex c2 "= sqrt(c1)";
  algorithm
    c2 := Complex(sqrt('abs'(c1))*Math.cos(arg(c1)/2), sqrt('abs'(c1))*Math.sin(arg(c1)/2));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex square root (principal square root) of the Complex input.</p>
</html>"));
  end 'sqrt';

  function 'max' "Return maximum element of complex vector"
    extends Modelica.Icons.Function;
    input Complex v[:] "Vector";
    output Complex result "Element of v with largest absolute value";
    output Integer index "v[index] has the largest absolute value";
  protected
    Real absv_i;
    Real absres;
  algorithm
    if size(v,1) > 0 then
      absres := 'abs'(v[1]);
      index  := 1;
      for i in 2:size(v,1) loop
        absv_i := 'abs'(v[i]);
        if absv_i > absres then
          absres := absv_i;
          index := i;
        end if;
      end for;
      result :=v[index];
    else
      result := Complex(0);
      index  := 0;
    end if;
    annotation(Documentation(info="<html>
<p>This function returns the largest element of the Complex input vector, defined by the Complex absolute.</p>
</html>"));
  end 'max';

  function 'min' "Return minimum element of complex vector"
    extends Modelica.Icons.Function;
    input Complex v[:] "Vector";
    output Complex result "Element of v with smallest absolute value";
    output Integer index "v[index] has the smallest absolute value";
  protected
    Real absv_i;
    Real absres;
  algorithm
    if size(v,1) > 0 then
      absres := 'abs'(v[1]);
      index  := 1;
      for i in 2:size(v,1) loop
        absv_i := 'abs'(v[i]);
        if absv_i < absres then
          absres := absv_i;
          index := i;
        end if;
      end for;
      result :=v[index];
    else
      result := Complex(0);
      index  := 0;
    end if;
    annotation(Documentation(info="<html>
<p>This function returns the smallest element of the Complex input vector, defined by the Complex absolute.</p>
</html>"));
  end 'min';

  function 'sum' "Return sum of complex vector"
    extends Modelica.Icons.Function;
    input Complex v[:] "Vector";
    output Complex result "Complex sum of vector elements";
  algorithm
    result:=Complex(sum(v[:].re), sum(v[:].im));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the Complex sum of the Complex input vector</p>
</html>"));
  end 'sum';

  function 'product' "Return product of complex vector"
    extends Modelica.Icons.Function;
    input Complex v[:] "Vector";
    output Complex result "Complex product of vector elements";
  algorithm
    result:=Complex(1);
    for i in 1:size(v,1) loop
      result:=result * v[i];
    end for;
    annotation(Documentation(info="<html>
<p>This function returns the Complex product of the Complex input vector</p>
</html>"));
  end 'product';

  annotation (Documentation(info="<html>
<p>
This package contains <b>basic mathematical functions</b>
operating on complex numbers (such as sin(..)),
as well as functions operating on vectors of complex numbers.
</p>

</html>"), Icon(coordinateSystem(extent={{-100,-100},{100,100}},
          preserveAspectRatio=false), graphics={
        Line(points={{32,-86},{32,88}}, color={175,175,175}),
        Line(points={{-84,2},{88,2}}, color={175,175,175}),
        Line(
          points={{-50,75},{-5,30}}),
        Line(
          points={{-50,30},{-5,75}}),
        Line(
          points={{-50,-30},{-5,-75}}),
        Line(
          points={{-50,-75},{-5,-30}})}));

end ComplexMath;
