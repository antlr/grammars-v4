begin

    comment

        John Walker's Floating Point Benchmark, derived from...

        Marinchip Interactive Lens Design System

        By John Walker
           http://www.fourmilab.ch/

        This program may be used, distributed, and modified freely as
        long as the origin information is preserved.

        This is a complete optical design_raytracing algorithm,
        stripped of its user interface and recast into ALGOL 60.
        It not only determines execution speed on an extremely
        floating point (including trig function) intensive
        real-world application, it checks accuracy on an algorithm
        that is exquisitely sensitive to errors. The performance of
        this program is typically far more sensitive to changes in
        the efficiency of the trigonometric library routines than the
        average floating point program.

        Ported from the Algol 68 language implementation in May 2014
        by John Walker.
    ;

    comment
        Variables and constants global to all procedures.
    ;
    
    integer a_line, b_line, c_line, d_line,
            e_line, f_line, g_prime_line,
            h_line;
    real array spectral_line[1:8];
    
    integer current_surfaces;
    real array test_case[1:4, 1:4];
    integer curvature_radius, index_of_refraction, dispersion,
        edge_thickness;
    
    real clear_aperture, aber_lspher, aberr_osc, aberr_lchrom,
         max_lspher, mac_osc, max_lchrom, radius_of_curvature,
         object_distance,ray_height, axis_slope_angle,
         from_index, to_index;
         
    integer number_of_iterations, iteration;
         
    integer paraxial, marginal_ray, paraxial_ray;
    
    comment
        The od_sa array holds the result from the main trace of
        paraxial and marginal_rays in D light.  The first subscript
        indicates whether the_ray is marginal or paraxial and the
        second selects the object_distance or axis_slope_angle.
    ;
    
    integer of_field, sa_field;
    real array od_sa[1:2, 1:2];
    
    real od_cline, od_fline;
    
    comment
        The cot function defined in terms of sin and cos
    ;
    
    real procedure cot(x);
        value x;
        real x; 
    begin
        cot := 1 / (sin(x) / cos(x))
    end cot;
    
    comment
        The arc_sin function defined in terms of arc tan
    ;
    
    real procedure arc_sin(x);
        value x;
        real x;
    begin
        arc_sin := 2 * arctan(x / (1 + sqrt(1 - x^2)))
    end arc_sin;
    
    comment
        The built-in procedure outreal() uses the equivalent of
        a C format "%.12g", which does not provide sufficient
        precision for the largest numbers we edit.  This
        brutal hack gets around this limitation by defining
        an outbigreal() procedure which uses the marst
        inline() mechanism to print its argument with a format
        of "%.11f" as used by other implementations of the
        benchmark.  This is only needed, and only used, when
        printing the object_distance for the marginal and
        paraxial_rays in D light.
        
        If you have trouble with this, just replace the body of
        this procedure with:
        
            outreal_16_11 (channel, x)
            
        but then you'll have to manually verify these values are
        rounded correctly.
        
        Usually, I would avoid using a compiler-specific feature
        like this in a benchmark, but since ALGOL 60 has no
        I/O facilities defined in the standard, they differ
        among almost all compilers, so any working version of the
        program is necessarily non-portable without modifying the
        I/O.
    ;
    
    procedure outbigreal(channel, x);
        value channel, x;
        integer channel;
        real x;
    begin
	outreal_16_11 (channel, x);
    end outbigreal;
    
    comment
    
        Calculate passage through surface
  
        If the variable paraxial is paraxial_ray, the trace through the
        surface will be done using the paraxial approximations.
        Otherwise, the normal trigonometric trace will be done.
  
        This procedure takes the following global inputs:
  
        radius_of_curvature     Radius of curvature of surface
                                being crossed.  If 0, surface is
                                plane.
  
        object_distance         Distance of object focus from
                                lens vertex.  If 0, incoming
                               _rays are parallel and
                                the following must be specified:
  
       ray_height              Height of_ray from axis.  Only
                                relevant if object_distance = 0
  
        axis_slope_angle        Angle incoming_ray makes with axis
                                at intercept
  
        from_index              Refractive index of medium being left
  
        to_index                Refractive index of medium being
                                entered.
  
        The outputs are the following global variables:
  
        object_distance         Distance from vertex to object focus
                                after refraction.
  
        axis_slope_angle        Angle incoming_ray makes with axis
                                at intercept after refraction.
    ;

    procedure transit_surface;
    begin
        real iang, rang, iang_sin, rang_sin,
             old_axis_slope_angle, sagitta;
             
        if paraxial = paraxial_ray then begin
            if radius_of_curvature != 0 then begin
                if object_distance = 0 then begin
                    axis_slope_angle := 0;
                    iang_sin :=ray_height / radius_of_curvature
                end else
                    iang_sin := ((object_distance -
                        radius_of_curvature) / radius_of_curvature) *
                        axis_slope_angle;
                rang_sin := (from_index / to_index) * iang_sin;
                old_axis_slope_angle := axis_slope_angle;
                axis_slope_angle := axis_slope_angle +
                    iang_sin - rang_sin;
                if object_distance != 0 then
                   ray_height := object_distance * old_axis_slope_angle;
                object_distance :=ray_height / axis_slope_angle
            end else begin
                object_distance := object_distance * (to_index / from_index);
                axis_slope_angle := axis_slope_angle * (from_index / to_index)
            end
        end else begin
            if radius_of_curvature != 0 then begin
                if object_distance = 0 then begin
                    axis_slope_angle := 0;
                    iang_sin :=ray_height / radius_of_curvature
                end else
                    iang_sin := ((object_distance -
                        radius_of_curvature) / radius_of_curvature) *
                        sin(axis_slope_angle);
                iang := arc_sin(iang_sin);
                rang_sin := (from_index / to_index) * iang_sin;
                old_axis_slope_angle := axis_slope_angle;
                axis_slope_angle := axis_slope_angle +
                    iang - arc_sin(rang_sin);
                sagitta := sin((old_axis_slope_angle + iang) / 2);
                sagitta := 2 * radius_of_curvature * (sagitta ^ 2);
                object_distance := ((radius_of_curvature *
                    sin(old_axis_slope_angle + iang)) *
                    cot(axis_slope_angle)) + sagitta
            end else begin
                rang := -arc_sin((from_index / to_index) *
                    sin(axis_slope_angle));
                object_distance := object_distance * ((to_index *
                    cos(-rang)) / (from_index *
                    cos(axis_slope_angle)));
                axis_slope_angle := -rang
           end

        end
    end transit_surface;
    
    comment
        Perform_ray trace for a given design for a specific
        spectral_line andray_height.  The caller specifies the
        desired spectral_line and ray_height.  The global
        object_distance is updated based upon tracing this
       _ray.
    ;
    
    procedure trace_line(line, ray_h);
        value line, ray_h;
        integer line;
        real ray_h;
    begin
        integer i;
        
        object_distance := 0;
        ray_height :=ray_h;
        from_index := 1;
        
        for i := 1 step 1 until current_surfaces do begin
            radius_of_curvature := test_case[i, curvature_radius];
            to_index := test_case[i, index_of_refraction];
            if to_index > 1 then
                to_index := to_index + ((spectral_line[d_line] -
                    spectral_line[line]) /
                    (spectral_line[c_line] - spectral_line[f_line])) *
                    ((test_case[i, index_of_refraction] - 1.0) /
                    test_case[i, dispersion]);
            transit_surface;
            from_index := to_index;
            if i < current_surfaces then
                object_distance := object_distance -
                    test_case[i, edge_thickness]
        end
    end trace_line;
    
    comment END GLOBAL DECLARATONS ;

    number_of_iterations:= 100000000;
    outstring (1, "Ready to begin John Walker's floating point accuracy and performance benchmark.");
    outinteger (1, number_of_iterations);
    outstring  (1, " will be made\n");
    comment Spectral_lines ;
        
    a_line := 1; b_line := 2; c_line := 3; d_line := 4;
    e_line := 5; f_line := 6; g_prime_line := 7;
    h_line := 8;
    
    spectral_line [a_line]       := 7621.0; 
    spectral_line [b_line]       := 6869.955; 
    spectral_line [c_line]       := 6562.8160; 
    spectral_line [d_line]       := 5895.944; 
    spectral_line [e_line]       := 5269.557; 
    spectral_line [f_line]       := 4861.344; 
    spectral_line [g_prime_line] := 4340.477; 
    spectral_line [h_line]       := 3968.494; 
    
    comment  Initialise the test_case array
    
             The  test_case used in this program is the design for a 4 inch
             f/12 achromatic telescope objective used as the example in Wyld's
             classic work on_ray tracing by hand, given in Amateur Telescope
             Making, Volume 3 (Volume 2 in the 1996 reprint edition). 
    ;
    
    current_surfaces := 4;
    
    curvature_radius := 1; index_of_refraction := 2; dispersion := 3;
        edge_thickness := 4;
        
    test_case[1, curvature_radius]    :=   27.05;
    test_case[1, index_of_refraction] :=    1.5137;
    test_case[1, dispersion]          :=   63.6;
    test_case[1, edge_thickness]      :=    0.52;
    
    test_case[2, curvature_radius]    :=  -16.68;
    test_case[2, index_of_refraction] :=    1.0;
    test_case[2, dispersion]          :=    0.0;
    test_case[2, edge_thickness]      :=    0.138;
    
    test_case[3, curvature_radius]    :=  -16.68;
    test_case[3, index_of_refraction] :=    1.6164;
    test_case[3, dispersion]          :=   36.7;
    test_case[3, edge_thickness]      :=    0.38;
    
    test_case[4, curvature_radius]    :=  -78.1;
    test_case[4, index_of_refraction] :=    1.0;
    test_case[4, dispersion]          :=    0.0;
    test_case[4, edge_thickness]      :=    0.0;
    
    marginal_ray := 1; paraxial_ray := 2;
    
    of_field := 1; sa_field := 2;
 
comment
    number_of_iterations:= 1;
    
    clear_aperture := 4;
    
    for iteration := 1 step 1 until number_of_iterations do begin
    
        for paraxial:= marginal_ray step 1 until paraxial_ray do begin
            trace_line(d_line, clear_aperture / 2);
            od_sa[paraxial, of_field] := object_distance;
            od_sa[paraxial, sa_field] := axis_slope_angle
        end;

        comment Trace marginal_ray in C ;

        paraxial := marginal_ray;
        trace_line(c_line, clear_aperture / 2);
        od_cline := object_distance;

        comment Trace marginal_ray in F ;

        trace_line(f_line, clear_aperture / 2);
        od_fline := object_distance;

        comment

            Compute aberrations of the design

            The longitudinal spherical aberration is just the
            difference between where the D_line comes to focus
            for paraxial and marginal_rays. ;

        aber_lspher := od_sa[paraxial_ray, of_field] -
            od_sa[marginal_ray, of_field];

        comment

            The offense against the sine condition is a measure
            of the degree of coma in the design.  We compute it
            as the lateral distance in the focal plane between
            where a paraxial_ray and marginal_ray in the D_line
            come to focus. ;

        aberr_osc := 1 - ((od_sa[paraxial_ray, of_field] *
                           od_sa[paraxial_ray, sa_field]) /
                           (sin(od_sa[marginal_ray, sa_field]) *
                            od_sa[marginal_ray, of_field]));

        comment

            The axial chromatic aberration is the distance between
            where marginal_rays in the C and F_lines come to focus. ;

        aberr_lchrom := od_fline - od_cline;

        comment

            Compute maximum acceptable values for each aberration

            Maximum longitudinal spherical aberration, which is
            also the maximum for axial chromatic aberration.  This
            is computed for the D_line. ;

        max_lspher := 0.0000926 / sin(od_sa[marginal_ray, sa_field]) ^ 2;
        max_lchrom := max_lspher;
        mac_osc := 0.0025
    end;
    
    comment Print the analysis of the_ray trace ;
    
    outstring  (1, "  Marginal_ray    ");
    outbigreal (1, od_sa[marginal_ray, of_field]);
    outstring  (1, "   ");
    outbigreal (1, od_sa[marginal_ray, sa_field]); outstring(1, "\n");
    
    outstring  (1, "  Paraxial_ray    ");
    outbigreal (1, od_sa[paraxial_ray, of_field]);
    outstring  (1, "   ");
    outbigreal (1, od_sa[paraxial_ray, sa_field]); outstring(1, "\n");
    
    outstring  (1, "Longitudinal spherical aberration: ");
    outbigreal (1, aber_lspher); outstring(1, "\n");
    outstring  (1, "    (Maximum permissible): ");
    outbigreal (1, max_lspher); outstring(1, "\n");
    
    outstring  (1, "Offense against sine condition (coma): ");
    outbigreal (1, aberr_osc); outstring(1, "\n");
    outstring  (1, "    (Maximum permissible): ");
    outbigreal (1, mac_osc); outstring(1, "\n");
    
    outstring  (1, "Axial chromatic aberration: ");
    outbigreal (1, aberr_lchrom); outstring(1, "\n");
    outstring  (1, "    (Maximum permissible): ");
    outbigreal (1, max_lchrom); outstring(1, "\n");

    outstring  (1,  "Measured run time in seconds should be divided by ");
    outinteger (1,   number_of_iterations / 1000);
    outstring  (1, " to normalise for reporting results.\n");
    outstring  (1,  "For archival results, adjust iteration count so the benchmark runs about five minutes.\n");

end
