within ;
operator record Complex "Complex number with overloaded operators"
//record Complex "Complex number with overloaded operators"
  replaceable Real re "Real part of complex number" annotation(Dialog);
  replaceable Real im "Imaginary part of complex number" annotation(Dialog);

  encapsulated operator 'constructor' "Constructor"
    function fromReal "Construct Complex from Real"
      import Complex;
      input Real re "Real part of complex number";
      input Real im=0 "Imaginary part of complex number";
      output Complex result(re=re, im=im) "Complex number";
    algorithm

      annotation(Inline=true, Documentation(info="<html>
<p>This function returns a Complex number defined by real part <i>re</i> and optional imaginary part <i>im</i> (default=0).</p>
</html>"));
    end fromReal;
    annotation (Documentation(info="<html>
<p>Here the constructor operator(s) is/are defined.</p>
</html>"), Icon(graphics={Rectangle(
            lineColor={200,200,200},
            fillColor={248,248,248},
            fillPattern=FillPattern.HorizontalCylinder,
            extent={{-100,-100},{100,100}},
            radius=25.0), Rectangle(
            lineColor={128,128,128},
            fillPattern=FillPattern.None,
            extent={{-100,-100},{100,100}},
            radius=25.0)}));
  end 'constructor';

  encapsulated operator function '0' "Zero-element of addition (= Complex(0))"
    import Complex;
    output Complex result "Complex(0)";
  algorithm
    result := Complex(0);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the zero-element of Complex, that is, Complex(0) = 0 + j*0.</p>
</html>"));
  end '0';

  encapsulated operator '-' "Unary and binary minus"
    function negate "Unary minus (multiply complex number by -1)"
      import Complex;
      input Complex c1 "Complex number";
      output Complex c2 "= -c1";
    algorithm
      c2 := Complex(-c1.re, -c1.im);
      annotation(Inline=true, Documentation(info="<html>
<p>This function returns the binary minus of the given Complex number.</p>
</html>"));
    end negate;

    function subtract "Subtract two complex numbers"
      import Complex;
      input Complex c1 "Complex number 1";
      input Complex c2 "Complex number 2";
      output Complex c3 "= c1 - c2";
    algorithm
      c3 := Complex(c1.re - c2.re, c1.im - c2.im);
      annotation(Inline=true, Documentation(info="<html>
<p>This function returns the difference of two given Complex numbers.</p>
</html>"));
    end subtract;
    annotation (Documentation(info="<html>
<p>Here the unary and binary minus operator(s) is/are defined.</p>
</html>"), Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
              {100,100}}), graphics={
          Rectangle(
            lineColor={200,200,200},
            fillColor={248,248,248},
            fillPattern=FillPattern.HorizontalCylinder,
            extent={{-100,-100},{100,100}},
            radius=25.0),
          Rectangle(
            lineColor={128,128,128},
            fillPattern=FillPattern.None,
            extent={{-100,-100},{100,100}},
            radius=25.0),
          Line(
            points={{-50,0},{50,0}},
            color={0,0,0},
            smooth=Smooth.None)}));
  end '-';

  encapsulated operator '*' "Multiplication"
    function multiply "Multiply two complex numbers"
      import Complex;
      input Complex c1 "Complex number 1";
      input Complex c2 "Complex number 2";
      output Complex c3 "= c1*c2";
    algorithm
      c3 := Complex(c1.re*c2.re - c1.im*c2.im, c1.re*c2.im + c1.im*c2.re);

    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the product of two given Complex numbers.</p>
</html>"));
    end multiply;

    function scalarProduct "Scalar product c1*c2 of two complex vectors"
      import Complex;
      input Complex c1[:] "Vector of Complex numbers 1";
      input Complex c2[size(c1,1)] "Vector of Complex numbers 2";
      output Complex c3 "= c1*c2";
    algorithm
      c3 :=Complex(0);
      for i in 1:size(c1,1) loop
         c3 :=c3 + c1[i]*c2[i];
         /*
       c3 :=Complex(c3.re + c1[i].re*c2[i].re - c1[i].im*c2[i].im,
                    c3.im + c1[i].re*c2[i].im + c1[i].im*c2[i].re);
       */
      end for;

    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the scalar product of two given arrays of Complex numbers.</p>
</html>"));
    end scalarProduct;
    annotation (
      Documentation(info="<html>
<p>Here the multiplication operator(s) is/are defined.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}}),
          graphics={
          Rectangle(
            lineColor={200,200,200},
            fillColor={248,248,248},
            fillPattern=FillPattern.HorizontalCylinder,
            extent={{-100,-100},{100,100}},
            radius=25.0),
          Rectangle(
            lineColor={128,128,128},
            fillPattern=FillPattern.None,
            extent={{-100,-100},{100,100}},
            radius=25.0),
          Line(
            points={{-42,36},{39,-34}},
            color={0,0,0},
            smooth=Smooth.None),
          Line(
            points={{-42,-35},{39,37}},
            color={0,0,0},
            smooth=Smooth.None),
          Line(
            points={{-55,1},{52,1}},
            color={0,0,0},
            smooth=Smooth.None),
          Line(
            points={{-1.5,55},{-2,-53}},
            color={0,0,0},
            smooth=Smooth.None)}));
  end '*';

  encapsulated operator function '+' "Add two complex numbers"
    import Complex;
    input Complex c1 "Complex number 1";
    input Complex c2 "Complex number 2";
    output Complex c3 "= c1 + c2";
  algorithm
    c3 := Complex(c1.re + c2.re, c1.im + c2.im);
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the sum of two given Complex numbers.</p>
</html>"));
  end '+';

  encapsulated operator function '/' "Divide two complex numbers"
    import Complex;
    input Complex c1 "Complex number 1";
    input Complex c2 "Complex number 2";
    output Complex c3 "= c1/c2";
  algorithm
    c3 := Complex((+c1.re*c2.re + c1.im*c2.im)/(c2.re*c2.re + c2.im*c2.im),
                  (-c1.re*c2.im + c1.im*c2.re)/(c2.re*c2.re + c2.im*c2.im));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the quotient of two given Complex numbers.</p>
</html>"));
  end '/';

  encapsulated operator function '^' "Complex power of complex number"
    import Complex;
    input Complex c1 "Complex number";
    input Complex c2 "Complex exponent";
    output Complex c3 "= c1^c2";
  protected
    Real lnz=0.5*log(c1.re*c1.re + c1.im*c1.im);
    Real phi=atan2(c1.im, c1.re);
    Real re=lnz*c2.re - phi*c2.im;
    Real im=lnz*c2.im + phi*c2.re;
  algorithm
    c3 := Complex(exp(re)*cos(im), exp(re)*sin(im));
    annotation(Inline=true, Documentation(info="<html>
<p>This function returns the given Complex numbers c1 to the power of the Complex number c2.</p>
</html>"));
  end '^';

  encapsulated operator function '=='
    "Test whether two complex numbers are identical"
    import Complex;
    input Complex c1 "Complex number 1";
    input Complex c2 "Complex number 2";
    output Boolean result "c1 == c2";
  algorithm
    result := c1.re == c2.re and c1.im == c2.im;
    annotation(Inline=true, Documentation(info="<html>
<p>This function tests whether two given Complex numbers are equal.</p>
</html>"));
  end '==';

  encapsulated operator function '<>'
    "Test whether two complex numbers are not identical"
    import Complex;
    input Complex c1 "Complex number 1";
    input Complex c2 "Complex number 2";
    output Boolean result "c1 <> c2";
  algorithm
    result := c1.re <> c2.re or c1.im <> c2.im;
    annotation(Inline=true, Documentation(info="<html>
    <p>This function tests whether two given Complex numbers are not equal.</p>
</html>"));
  end '<>';

  encapsulated operator function 'String'
    "Transform Complex number into a String representation"
    import Complex;
    input Complex c
      "Complex number to be transformed in a String representation";
    input String name="j"
      "Name of variable representing sqrt(-1) in the string";
    input Integer significantDigits=6
      "Number of significant digits that are shown";
    output String s="";
  algorithm
    s := String(c.re, significantDigits=significantDigits);
    if c.im <> 0 then
      if c.im > 0 then
        s := s + " + ";
      else
        s := s + " - ";
      end if;
      s := s + String(abs(c.im), significantDigits=significantDigits) + "*" + name;
    end if;
    annotation(Inline=true, Documentation(info="<html>
<p>This function converts a given Complex number to String representation.</p>
</html>"));
  end 'String';


annotation (Protection(access=Access.hide),
version="3.2.2",
versionBuild=0,
versionDate="2016-01-15",
dateModified = "2016-01-15 08:44:41Z",
revisionId="$Id::                                       $",
conversion(
 noneFromVersion="3.2.1",
 noneFromVersion="1.0",
 noneFromVersion="1.1"),
Documentation(info="<html>
<p>Complex number defined as a record containing real and imaginary part, utilizing operator overloading.</p>
</html>"),
    Icon(graphics={Rectangle(
          lineColor={160,160,164},
          fillColor={160,160,164},
          fillPattern=FillPattern.Solid,
          extent={{-100,-100},{100,100}},
          radius=25.0), Text(
          lineColor={255,255,255},
          extent={{-90,-50},{90,50}},
          textString="C")}));

end Complex;
