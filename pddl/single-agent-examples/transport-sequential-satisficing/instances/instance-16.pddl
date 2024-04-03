; Transport two-cities-sequential-62nodes-1000size-4degree-100mindistance-4trucks-22packages-2013seed

(define (problem transport-two-cities-sequential-62nodes-1000size-4degree-100mindistance-4trucks-22packages-2013seed)
 (:domain transport)
 (:objects
  city-1-loc-1 - location
  city-2-loc-1 - location
  city-1-loc-2 - location
  city-2-loc-2 - location
  city-1-loc-3 - location
  city-2-loc-3 - location
  city-1-loc-4 - location
  city-2-loc-4 - location
  city-1-loc-5 - location
  city-2-loc-5 - location
  city-1-loc-6 - location
  city-2-loc-6 - location
  city-1-loc-7 - location
  city-2-loc-7 - location
  city-1-loc-8 - location
  city-2-loc-8 - location
  city-1-loc-9 - location
  city-2-loc-9 - location
  city-1-loc-10 - location
  city-2-loc-10 - location
  city-1-loc-11 - location
  city-2-loc-11 - location
  city-1-loc-12 - location
  city-2-loc-12 - location
  city-1-loc-13 - location
  city-2-loc-13 - location
  city-1-loc-14 - location
  city-2-loc-14 - location
  city-1-loc-15 - location
  city-2-loc-15 - location
  city-1-loc-16 - location
  city-2-loc-16 - location
  city-1-loc-17 - location
  city-2-loc-17 - location
  city-1-loc-18 - location
  city-2-loc-18 - location
  city-1-loc-19 - location
  city-2-loc-19 - location
  city-1-loc-20 - location
  city-2-loc-20 - location
  city-1-loc-21 - location
  city-2-loc-21 - location
  city-1-loc-22 - location
  city-2-loc-22 - location
  city-1-loc-23 - location
  city-2-loc-23 - location
  city-1-loc-24 - location
  city-2-loc-24 - location
  city-1-loc-25 - location
  city-2-loc-25 - location
  city-1-loc-26 - location
  city-2-loc-26 - location
  city-1-loc-27 - location
  city-2-loc-27 - location
  city-1-loc-28 - location
  city-2-loc-28 - location
  city-1-loc-29 - location
  city-2-loc-29 - location
  city-1-loc-30 - location
  city-2-loc-30 - location
  city-1-loc-31 - location
  city-2-loc-31 - location
  city-1-loc-32 - location
  city-2-loc-32 - location
  city-1-loc-33 - location
  city-2-loc-33 - location
  city-1-loc-34 - location
  city-2-loc-34 - location
  city-1-loc-35 - location
  city-2-loc-35 - location
  city-1-loc-36 - location
  city-2-loc-36 - location
  city-1-loc-37 - location
  city-2-loc-37 - location
  city-1-loc-38 - location
  city-2-loc-38 - location
  city-1-loc-39 - location
  city-2-loc-39 - location
  city-1-loc-40 - location
  city-2-loc-40 - location
  city-1-loc-41 - location
  city-2-loc-41 - location
  city-1-loc-42 - location
  city-2-loc-42 - location
  city-1-loc-43 - location
  city-2-loc-43 - location
  city-1-loc-44 - location
  city-2-loc-44 - location
  city-1-loc-45 - location
  city-2-loc-45 - location
  city-1-loc-46 - location
  city-2-loc-46 - location
  city-1-loc-47 - location
  city-2-loc-47 - location
  city-1-loc-48 - location
  city-2-loc-48 - location
  city-1-loc-49 - location
  city-2-loc-49 - location
  city-1-loc-50 - location
  city-2-loc-50 - location
  city-1-loc-51 - location
  city-2-loc-51 - location
  city-1-loc-52 - location
  city-2-loc-52 - location
  city-1-loc-53 - location
  city-2-loc-53 - location
  city-1-loc-54 - location
  city-2-loc-54 - location
  city-1-loc-55 - location
  city-2-loc-55 - location
  city-1-loc-56 - location
  city-2-loc-56 - location
  city-1-loc-57 - location
  city-2-loc-57 - location
  city-1-loc-58 - location
  city-2-loc-58 - location
  city-1-loc-59 - location
  city-2-loc-59 - location
  city-1-loc-60 - location
  city-2-loc-60 - location
  city-1-loc-61 - location
  city-2-loc-61 - location
  city-1-loc-62 - location
  city-2-loc-62 - location
  truck-1 - vehicle
  truck-2 - vehicle
  truck-3 - vehicle
  truck-4 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
  package-4 - package
  package-5 - package
  package-6 - package
  package-7 - package
  package-8 - package
  package-9 - package
  package-10 - package
  package-11 - package
  package-12 - package
  package-13 - package
  package-14 - package
  package-15 - package
  package-16 - package
  package-17 - package
  package-18 - package
  package-19 - package
  package-20 - package
  package-21 - package
  package-22 - package
  capacity-0 - capacity-number
  capacity-1 - capacity-number
  capacity-2 - capacity-number
  capacity-3 - capacity-number
  capacity-4 - capacity-number
 )
 (:init
  (= (total-cost) 0)
  (capacity-predecessor capacity-0 capacity-1)
  (capacity-predecessor capacity-1 capacity-2)
  (capacity-predecessor capacity-2 capacity-3)
  (capacity-predecessor capacity-3 capacity-4)
  ; 554,565 -> 512,409
  (road city-1-loc-5 city-1-loc-4)
  (= (road-length city-1-loc-5 city-1-loc-4) 17)
  ; 512,409 -> 554,565
  (road city-1-loc-4 city-1-loc-5)
  (= (road-length city-1-loc-4 city-1-loc-5) 17)
  ; 700,557 -> 554,565
  (road city-1-loc-9 city-1-loc-5)
  (= (road-length city-1-loc-9 city-1-loc-5) 15)
  ; 554,565 -> 700,557
  (road city-1-loc-5 city-1-loc-9)
  (= (road-length city-1-loc-5 city-1-loc-9) 15)
  ; 835,584 -> 700,557
  (road city-1-loc-12 city-1-loc-9)
  (= (road-length city-1-loc-12 city-1-loc-9) 14)
  ; 700,557 -> 835,584
  (road city-1-loc-9 city-1-loc-12)
  (= (road-length city-1-loc-9 city-1-loc-12) 14)
  ; 835,584 -> 944,697
  (road city-1-loc-12 city-1-loc-10)
  (= (road-length city-1-loc-12 city-1-loc-10) 16)
  ; 944,697 -> 835,584
  (road city-1-loc-10 city-1-loc-12)
  (= (road-length city-1-loc-10 city-1-loc-12) 16)
  ; 389,798 -> 401,900
  (road city-1-loc-14 city-1-loc-2)
  (= (road-length city-1-loc-14 city-1-loc-2) 11)
  ; 401,900 -> 389,798
  (road city-1-loc-2 city-1-loc-14)
  (= (road-length city-1-loc-2 city-1-loc-14) 11)
  ; 625,642 -> 554,565
  (road city-1-loc-15 city-1-loc-5)
  (= (road-length city-1-loc-15 city-1-loc-5) 11)
  ; 554,565 -> 625,642
  (road city-1-loc-5 city-1-loc-15)
  (= (road-length city-1-loc-5 city-1-loc-15) 11)
  ; 625,642 -> 700,557
  (road city-1-loc-15 city-1-loc-9)
  (= (road-length city-1-loc-15 city-1-loc-9) 12)
  ; 700,557 -> 625,642
  (road city-1-loc-9 city-1-loc-15)
  (= (road-length city-1-loc-9 city-1-loc-15) 12)
  ; 922,875 -> 765,898
  (road city-1-loc-16 city-1-loc-6)
  (= (road-length city-1-loc-16 city-1-loc-6) 16)
  ; 765,898 -> 922,875
  (road city-1-loc-6 city-1-loc-16)
  (= (road-length city-1-loc-6 city-1-loc-16) 16)
  ; 13,248 -> 55,393
  (road city-1-loc-17 city-1-loc-7)
  (= (road-length city-1-loc-17 city-1-loc-7) 16)
  ; 55,393 -> 13,248
  (road city-1-loc-7 city-1-loc-17)
  (= (road-length city-1-loc-7 city-1-loc-17) 16)
  ; 520,164 -> 425,51
  (road city-1-loc-18 city-1-loc-1)
  (= (road-length city-1-loc-18 city-1-loc-1) 15)
  ; 425,51 -> 520,164
  (road city-1-loc-1 city-1-loc-18)
  (= (road-length city-1-loc-1 city-1-loc-18) 15)
  ; 626,869 -> 765,898
  (road city-1-loc-19 city-1-loc-6)
  (= (road-length city-1-loc-19 city-1-loc-6) 15)
  ; 765,898 -> 626,869
  (road city-1-loc-6 city-1-loc-19)
  (= (road-length city-1-loc-6 city-1-loc-19) 15)
  ; 148,205 -> 276,182
  (road city-1-loc-20 city-1-loc-11)
  (= (road-length city-1-loc-20 city-1-loc-11) 13)
  ; 276,182 -> 148,205
  (road city-1-loc-11 city-1-loc-20)
  (= (road-length city-1-loc-11 city-1-loc-20) 13)
  ; 148,205 -> 13,248
  (road city-1-loc-20 city-1-loc-17)
  (= (road-length city-1-loc-20 city-1-loc-17) 15)
  ; 13,248 -> 148,205
  (road city-1-loc-17 city-1-loc-20)
  (= (road-length city-1-loc-17 city-1-loc-20) 15)
  ; 782,127 -> 727,217
  (road city-1-loc-22 city-1-loc-13)
  (= (road-length city-1-loc-22 city-1-loc-13) 11)
  ; 727,217 -> 782,127
  (road city-1-loc-13 city-1-loc-22)
  (= (road-length city-1-loc-13 city-1-loc-22) 11)
  ; 815,689 -> 944,697
  (road city-1-loc-23 city-1-loc-10)
  (= (road-length city-1-loc-23 city-1-loc-10) 13)
  ; 944,697 -> 815,689
  (road city-1-loc-10 city-1-loc-23)
  (= (road-length city-1-loc-10 city-1-loc-23) 13)
  ; 815,689 -> 835,584
  (road city-1-loc-23 city-1-loc-12)
  (= (road-length city-1-loc-23 city-1-loc-12) 11)
  ; 835,584 -> 815,689
  (road city-1-loc-12 city-1-loc-23)
  (= (road-length city-1-loc-12 city-1-loc-23) 11)
  ; 267,298 -> 276,182
  (road city-1-loc-24 city-1-loc-11)
  (= (road-length city-1-loc-24 city-1-loc-11) 12)
  ; 276,182 -> 267,298
  (road city-1-loc-11 city-1-loc-24)
  (= (road-length city-1-loc-11 city-1-loc-24) 12)
  ; 267,298 -> 148,205
  (road city-1-loc-24 city-1-loc-20)
  (= (road-length city-1-loc-24 city-1-loc-20) 16)
  ; 148,205 -> 267,298
  (road city-1-loc-20 city-1-loc-24)
  (= (road-length city-1-loc-20 city-1-loc-24) 16)
  ; 371,493 -> 512,409
  (road city-1-loc-26 city-1-loc-4)
  (= (road-length city-1-loc-26 city-1-loc-4) 17)
  ; 512,409 -> 371,493
  (road city-1-loc-4 city-1-loc-26)
  (= (road-length city-1-loc-4 city-1-loc-26) 17)
  ; 849,242 -> 727,217
  (road city-1-loc-27 city-1-loc-13)
  (= (road-length city-1-loc-27 city-1-loc-13) 13)
  ; 727,217 -> 849,242
  (road city-1-loc-13 city-1-loc-27)
  (= (road-length city-1-loc-13 city-1-loc-27) 13)
  ; 849,242 -> 782,127
  (road city-1-loc-27 city-1-loc-22)
  (= (road-length city-1-loc-27 city-1-loc-22) 14)
  ; 782,127 -> 849,242
  (road city-1-loc-22 city-1-loc-27)
  (= (road-length city-1-loc-22 city-1-loc-27) 14)
  ; 554,978 -> 401,900
  (road city-1-loc-28 city-1-loc-2)
  (= (road-length city-1-loc-28 city-1-loc-2) 18)
  ; 401,900 -> 554,978
  (road city-1-loc-2 city-1-loc-28)
  (= (road-length city-1-loc-2 city-1-loc-28) 18)
  ; 554,978 -> 626,869
  (road city-1-loc-28 city-1-loc-19)
  (= (road-length city-1-loc-28 city-1-loc-19) 14)
  ; 626,869 -> 554,978
  (road city-1-loc-19 city-1-loc-28)
  (= (road-length city-1-loc-19 city-1-loc-28) 14)
  ; 216,597 -> 126,655
  (road city-1-loc-31 city-1-loc-29)
  (= (road-length city-1-loc-31 city-1-loc-29) 11)
  ; 126,655 -> 216,597
  (road city-1-loc-29 city-1-loc-31)
  (= (road-length city-1-loc-29 city-1-loc-31) 11)
  ; 262,415 -> 267,298
  (road city-1-loc-32 city-1-loc-24)
  (= (road-length city-1-loc-32 city-1-loc-24) 12)
  ; 267,298 -> 262,415
  (road city-1-loc-24 city-1-loc-32)
  (= (road-length city-1-loc-24 city-1-loc-32) 12)
  ; 262,415 -> 371,493
  (road city-1-loc-32 city-1-loc-26)
  (= (road-length city-1-loc-32 city-1-loc-26) 14)
  ; 371,493 -> 262,415
  (road city-1-loc-26 city-1-loc-32)
  (= (road-length city-1-loc-26 city-1-loc-32) 14)
  ; 206,48 -> 276,182
  (road city-1-loc-33 city-1-loc-11)
  (= (road-length city-1-loc-33 city-1-loc-11) 16)
  ; 276,182 -> 206,48
  (road city-1-loc-11 city-1-loc-33)
  (= (road-length city-1-loc-11 city-1-loc-33) 16)
  ; 206,48 -> 148,205
  (road city-1-loc-33 city-1-loc-20)
  (= (road-length city-1-loc-33 city-1-loc-20) 17)
  ; 148,205 -> 206,48
  (road city-1-loc-20 city-1-loc-33)
  (= (road-length city-1-loc-20 city-1-loc-33) 17)
  ; 206,48 -> 87,35
  (road city-1-loc-33 city-1-loc-21)
  (= (road-length city-1-loc-33 city-1-loc-21) 12)
  ; 87,35 -> 206,48
  (road city-1-loc-21 city-1-loc-33)
  (= (road-length city-1-loc-21 city-1-loc-33) 12)
  ; 192,840 -> 81,825
  (road city-1-loc-34 city-1-loc-8)
  (= (road-length city-1-loc-34 city-1-loc-8) 12)
  ; 81,825 -> 192,840
  (road city-1-loc-8 city-1-loc-34)
  (= (road-length city-1-loc-8 city-1-loc-34) 12)
  ; 956,245 -> 849,242
  (road city-1-loc-35 city-1-loc-27)
  (= (road-length city-1-loc-35 city-1-loc-27) 11)
  ; 849,242 -> 956,245
  (road city-1-loc-27 city-1-loc-35)
  (= (road-length city-1-loc-27 city-1-loc-35) 11)
  ; 997,974 -> 922,875
  (road city-1-loc-36 city-1-loc-16)
  (= (road-length city-1-loc-36 city-1-loc-16) 13)
  ; 922,875 -> 997,974
  (road city-1-loc-16 city-1-loc-36)
  (= (road-length city-1-loc-16 city-1-loc-36) 13)
  ; 419,360 -> 512,409
  (road city-1-loc-37 city-1-loc-4)
  (= (road-length city-1-loc-37 city-1-loc-4) 11)
  ; 512,409 -> 419,360
  (road city-1-loc-4 city-1-loc-37)
  (= (road-length city-1-loc-4 city-1-loc-37) 11)
  ; 419,360 -> 267,298
  (road city-1-loc-37 city-1-loc-24)
  (= (road-length city-1-loc-37 city-1-loc-24) 17)
  ; 267,298 -> 419,360
  (road city-1-loc-24 city-1-loc-37)
  (= (road-length city-1-loc-24 city-1-loc-37) 17)
  ; 419,360 -> 371,493
  (road city-1-loc-37 city-1-loc-26)
  (= (road-length city-1-loc-37 city-1-loc-26) 15)
  ; 371,493 -> 419,360
  (road city-1-loc-26 city-1-loc-37)
  (= (road-length city-1-loc-26 city-1-loc-37) 15)
  ; 419,360 -> 262,415
  (road city-1-loc-37 city-1-loc-32)
  (= (road-length city-1-loc-37 city-1-loc-32) 17)
  ; 262,415 -> 419,360
  (road city-1-loc-32 city-1-loc-37)
  (= (road-length city-1-loc-32 city-1-loc-37) 17)
  ; 553,70 -> 425,51
  (road city-1-loc-38 city-1-loc-1)
  (= (road-length city-1-loc-38 city-1-loc-1) 13)
  ; 425,51 -> 553,70
  (road city-1-loc-1 city-1-loc-38)
  (= (road-length city-1-loc-1 city-1-loc-38) 13)
  ; 553,70 -> 520,164
  (road city-1-loc-38 city-1-loc-18)
  (= (road-length city-1-loc-38 city-1-loc-18) 10)
  ; 520,164 -> 553,70
  (road city-1-loc-18 city-1-loc-38)
  (= (road-length city-1-loc-18 city-1-loc-38) 10)
  ; 968,139 -> 849,242
  (road city-1-loc-39 city-1-loc-27)
  (= (road-length city-1-loc-39 city-1-loc-27) 16)
  ; 849,242 -> 968,139
  (road city-1-loc-27 city-1-loc-39)
  (= (road-length city-1-loc-27 city-1-loc-39) 16)
  ; 968,139 -> 969,3
  (road city-1-loc-39 city-1-loc-30)
  (= (road-length city-1-loc-39 city-1-loc-30) 14)
  ; 969,3 -> 968,139
  (road city-1-loc-30 city-1-loc-39)
  (= (road-length city-1-loc-30 city-1-loc-39) 14)
  ; 968,139 -> 956,245
  (road city-1-loc-39 city-1-loc-35)
  (= (road-length city-1-loc-39 city-1-loc-35) 11)
  ; 956,245 -> 968,139
  (road city-1-loc-35 city-1-loc-39)
  (= (road-length city-1-loc-35 city-1-loc-39) 11)
  ; 336,637 -> 389,798
  (road city-1-loc-40 city-1-loc-14)
  (= (road-length city-1-loc-40 city-1-loc-14) 17)
  ; 389,798 -> 336,637
  (road city-1-loc-14 city-1-loc-40)
  (= (road-length city-1-loc-14 city-1-loc-40) 17)
  ; 336,637 -> 371,493
  (road city-1-loc-40 city-1-loc-26)
  (= (road-length city-1-loc-40 city-1-loc-26) 15)
  ; 371,493 -> 336,637
  (road city-1-loc-26 city-1-loc-40)
  (= (road-length city-1-loc-26 city-1-loc-40) 15)
  ; 336,637 -> 216,597
  (road city-1-loc-40 city-1-loc-31)
  (= (road-length city-1-loc-40 city-1-loc-31) 13)
  ; 216,597 -> 336,637
  (road city-1-loc-31 city-1-loc-40)
  (= (road-length city-1-loc-31 city-1-loc-40) 13)
  ; 867,49 -> 782,127
  (road city-1-loc-41 city-1-loc-22)
  (= (road-length city-1-loc-41 city-1-loc-22) 12)
  ; 782,127 -> 867,49
  (road city-1-loc-22 city-1-loc-41)
  (= (road-length city-1-loc-22 city-1-loc-41) 12)
  ; 867,49 -> 969,3
  (road city-1-loc-41 city-1-loc-30)
  (= (road-length city-1-loc-41 city-1-loc-30) 12)
  ; 969,3 -> 867,49
  (road city-1-loc-30 city-1-loc-41)
  (= (road-length city-1-loc-30 city-1-loc-41) 12)
  ; 867,49 -> 968,139
  (road city-1-loc-41 city-1-loc-39)
  (= (road-length city-1-loc-41 city-1-loc-39) 14)
  ; 968,139 -> 867,49
  (road city-1-loc-39 city-1-loc-41)
  (= (road-length city-1-loc-39 city-1-loc-41) 14)
  ; 303,746 -> 389,798
  (road city-1-loc-42 city-1-loc-14)
  (= (road-length city-1-loc-42 city-1-loc-14) 10)
  ; 389,798 -> 303,746
  (road city-1-loc-14 city-1-loc-42)
  (= (road-length city-1-loc-14 city-1-loc-42) 10)
  ; 303,746 -> 192,840
  (road city-1-loc-42 city-1-loc-34)
  (= (road-length city-1-loc-42 city-1-loc-34) 15)
  ; 192,840 -> 303,746
  (road city-1-loc-34 city-1-loc-42)
  (= (road-length city-1-loc-34 city-1-loc-42) 15)
  ; 303,746 -> 336,637
  (road city-1-loc-42 city-1-loc-40)
  (= (road-length city-1-loc-42 city-1-loc-40) 12)
  ; 336,637 -> 303,746
  (road city-1-loc-40 city-1-loc-42)
  (= (road-length city-1-loc-40 city-1-loc-42) 12)
  ; 168,969 -> 81,825
  (road city-1-loc-43 city-1-loc-8)
  (= (road-length city-1-loc-43 city-1-loc-8) 17)
  ; 81,825 -> 168,969
  (road city-1-loc-8 city-1-loc-43)
  (= (road-length city-1-loc-8 city-1-loc-43) 17)
  ; 168,969 -> 192,840
  (road city-1-loc-43 city-1-loc-34)
  (= (road-length city-1-loc-43 city-1-loc-34) 14)
  ; 192,840 -> 168,969
  (road city-1-loc-34 city-1-loc-43)
  (= (road-length city-1-loc-34 city-1-loc-43) 14)
  ; 654,75 -> 727,217
  (road city-1-loc-44 city-1-loc-13)
  (= (road-length city-1-loc-44 city-1-loc-13) 16)
  ; 727,217 -> 654,75
  (road city-1-loc-13 city-1-loc-44)
  (= (road-length city-1-loc-13 city-1-loc-44) 16)
  ; 654,75 -> 520,164
  (road city-1-loc-44 city-1-loc-18)
  (= (road-length city-1-loc-44 city-1-loc-18) 17)
  ; 520,164 -> 654,75
  (road city-1-loc-18 city-1-loc-44)
  (= (road-length city-1-loc-18 city-1-loc-44) 17)
  ; 654,75 -> 782,127
  (road city-1-loc-44 city-1-loc-22)
  (= (road-length city-1-loc-44 city-1-loc-22) 14)
  ; 782,127 -> 654,75
  (road city-1-loc-22 city-1-loc-44)
  (= (road-length city-1-loc-22 city-1-loc-44) 14)
  ; 654,75 -> 553,70
  (road city-1-loc-44 city-1-loc-38)
  (= (road-length city-1-loc-44 city-1-loc-38) 11)
  ; 553,70 -> 654,75
  (road city-1-loc-38 city-1-loc-44)
  (= (road-length city-1-loc-38 city-1-loc-44) 11)
  ; 527,842 -> 401,900
  (road city-1-loc-45 city-1-loc-2)
  (= (road-length city-1-loc-45 city-1-loc-2) 14)
  ; 401,900 -> 527,842
  (road city-1-loc-2 city-1-loc-45)
  (= (road-length city-1-loc-2 city-1-loc-45) 14)
  ; 527,842 -> 389,798
  (road city-1-loc-45 city-1-loc-14)
  (= (road-length city-1-loc-45 city-1-loc-14) 15)
  ; 389,798 -> 527,842
  (road city-1-loc-14 city-1-loc-45)
  (= (road-length city-1-loc-14 city-1-loc-45) 15)
  ; 527,842 -> 626,869
  (road city-1-loc-45 city-1-loc-19)
  (= (road-length city-1-loc-45 city-1-loc-19) 11)
  ; 626,869 -> 527,842
  (road city-1-loc-19 city-1-loc-45)
  (= (road-length city-1-loc-19 city-1-loc-45) 11)
  ; 527,842 -> 554,978
  (road city-1-loc-45 city-1-loc-28)
  (= (road-length city-1-loc-45 city-1-loc-28) 14)
  ; 554,978 -> 527,842
  (road city-1-loc-28 city-1-loc-45)
  (= (road-length city-1-loc-28 city-1-loc-45) 14)
  ; 681,982 -> 765,898
  (road city-1-loc-46 city-1-loc-6)
  (= (road-length city-1-loc-46 city-1-loc-6) 12)
  ; 765,898 -> 681,982
  (road city-1-loc-6 city-1-loc-46)
  (= (road-length city-1-loc-6 city-1-loc-46) 12)
  ; 681,982 -> 626,869
  (road city-1-loc-46 city-1-loc-19)
  (= (road-length city-1-loc-46 city-1-loc-19) 13)
  ; 626,869 -> 681,982
  (road city-1-loc-19 city-1-loc-46)
  (= (road-length city-1-loc-19 city-1-loc-46) 13)
  ; 681,982 -> 554,978
  (road city-1-loc-46 city-1-loc-28)
  (= (road-length city-1-loc-46 city-1-loc-28) 13)
  ; 554,978 -> 681,982
  (road city-1-loc-28 city-1-loc-46)
  (= (road-length city-1-loc-28 city-1-loc-46) 13)
  ; 623,177 -> 727,217
  (road city-1-loc-47 city-1-loc-13)
  (= (road-length city-1-loc-47 city-1-loc-13) 12)
  ; 727,217 -> 623,177
  (road city-1-loc-13 city-1-loc-47)
  (= (road-length city-1-loc-13 city-1-loc-47) 12)
  ; 623,177 -> 520,164
  (road city-1-loc-47 city-1-loc-18)
  (= (road-length city-1-loc-47 city-1-loc-18) 11)
  ; 520,164 -> 623,177
  (road city-1-loc-18 city-1-loc-47)
  (= (road-length city-1-loc-18 city-1-loc-47) 11)
  ; 623,177 -> 782,127
  (road city-1-loc-47 city-1-loc-22)
  (= (road-length city-1-loc-47 city-1-loc-22) 17)
  ; 782,127 -> 623,177
  (road city-1-loc-22 city-1-loc-47)
  (= (road-length city-1-loc-22 city-1-loc-47) 17)
  ; 623,177 -> 553,70
  (road city-1-loc-47 city-1-loc-38)
  (= (road-length city-1-loc-47 city-1-loc-38) 13)
  ; 553,70 -> 623,177
  (road city-1-loc-38 city-1-loc-47)
  (= (road-length city-1-loc-38 city-1-loc-47) 13)
  ; 623,177 -> 654,75
  (road city-1-loc-47 city-1-loc-44)
  (= (road-length city-1-loc-47 city-1-loc-44) 11)
  ; 654,75 -> 623,177
  (road city-1-loc-44 city-1-loc-47)
  (= (road-length city-1-loc-44 city-1-loc-47) 11)
  ; 557,286 -> 512,409
  (road city-1-loc-48 city-1-loc-4)
  (= (road-length city-1-loc-48 city-1-loc-4) 14)
  ; 512,409 -> 557,286
  (road city-1-loc-4 city-1-loc-48)
  (= (road-length city-1-loc-4 city-1-loc-48) 14)
  ; 557,286 -> 520,164
  (road city-1-loc-48 city-1-loc-18)
  (= (road-length city-1-loc-48 city-1-loc-18) 13)
  ; 520,164 -> 557,286
  (road city-1-loc-18 city-1-loc-48)
  (= (road-length city-1-loc-18 city-1-loc-48) 13)
  ; 557,286 -> 419,360
  (road city-1-loc-48 city-1-loc-37)
  (= (road-length city-1-loc-48 city-1-loc-37) 16)
  ; 419,360 -> 557,286
  (road city-1-loc-37 city-1-loc-48)
  (= (road-length city-1-loc-37 city-1-loc-48) 16)
  ; 557,286 -> 623,177
  (road city-1-loc-48 city-1-loc-47)
  (= (road-length city-1-loc-48 city-1-loc-47) 13)
  ; 623,177 -> 557,286
  (road city-1-loc-47 city-1-loc-48)
  (= (road-length city-1-loc-47 city-1-loc-48) 13)
  ; 907,360 -> 755,388
  (road city-1-loc-49 city-1-loc-3)
  (= (road-length city-1-loc-49 city-1-loc-3) 16)
  ; 755,388 -> 907,360
  (road city-1-loc-3 city-1-loc-49)
  (= (road-length city-1-loc-3 city-1-loc-49) 16)
  ; 907,360 -> 987,459
  (road city-1-loc-49 city-1-loc-25)
  (= (road-length city-1-loc-49 city-1-loc-25) 13)
  ; 987,459 -> 907,360
  (road city-1-loc-25 city-1-loc-49)
  (= (road-length city-1-loc-25 city-1-loc-49) 13)
  ; 907,360 -> 849,242
  (road city-1-loc-49 city-1-loc-27)
  (= (road-length city-1-loc-49 city-1-loc-27) 14)
  ; 849,242 -> 907,360
  (road city-1-loc-27 city-1-loc-49)
  (= (road-length city-1-loc-27 city-1-loc-49) 14)
  ; 907,360 -> 956,245
  (road city-1-loc-49 city-1-loc-35)
  (= (road-length city-1-loc-49 city-1-loc-35) 13)
  ; 956,245 -> 907,360
  (road city-1-loc-35 city-1-loc-49)
  (= (road-length city-1-loc-35 city-1-loc-49) 13)
  ; 285,985 -> 401,900
  (road city-1-loc-50 city-1-loc-2)
  (= (road-length city-1-loc-50 city-1-loc-2) 15)
  ; 401,900 -> 285,985
  (road city-1-loc-2 city-1-loc-50)
  (= (road-length city-1-loc-2 city-1-loc-50) 15)
  ; 285,985 -> 192,840
  (road city-1-loc-50 city-1-loc-34)
  (= (road-length city-1-loc-50 city-1-loc-34) 18)
  ; 192,840 -> 285,985
  (road city-1-loc-34 city-1-loc-50)
  (= (road-length city-1-loc-34 city-1-loc-50) 18)
  ; 285,985 -> 168,969
  (road city-1-loc-50 city-1-loc-43)
  (= (road-length city-1-loc-50 city-1-loc-43) 12)
  ; 168,969 -> 285,985
  (road city-1-loc-43 city-1-loc-50)
  (= (road-length city-1-loc-43 city-1-loc-50) 12)
  ; 22,566 -> 126,655
  (road city-1-loc-51 city-1-loc-29)
  (= (road-length city-1-loc-51 city-1-loc-29) 14)
  ; 126,655 -> 22,566
  (road city-1-loc-29 city-1-loc-51)
  (= (road-length city-1-loc-29 city-1-loc-51) 14)
  ; 385,168 -> 425,51
  (road city-1-loc-52 city-1-loc-1)
  (= (road-length city-1-loc-52 city-1-loc-1) 13)
  ; 425,51 -> 385,168
  (road city-1-loc-1 city-1-loc-52)
  (= (road-length city-1-loc-1 city-1-loc-52) 13)
  ; 385,168 -> 276,182
  (road city-1-loc-52 city-1-loc-11)
  (= (road-length city-1-loc-52 city-1-loc-11) 11)
  ; 276,182 -> 385,168
  (road city-1-loc-11 city-1-loc-52)
  (= (road-length city-1-loc-11 city-1-loc-52) 11)
  ; 385,168 -> 520,164
  (road city-1-loc-52 city-1-loc-18)
  (= (road-length city-1-loc-52 city-1-loc-18) 14)
  ; 520,164 -> 385,168
  (road city-1-loc-18 city-1-loc-52)
  (= (road-length city-1-loc-18 city-1-loc-52) 14)
  ; 160,449 -> 55,393
  (road city-1-loc-53 city-1-loc-7)
  (= (road-length city-1-loc-53 city-1-loc-7) 12)
  ; 55,393 -> 160,449
  (road city-1-loc-7 city-1-loc-53)
  (= (road-length city-1-loc-7 city-1-loc-53) 12)
  ; 160,449 -> 216,597
  (road city-1-loc-53 city-1-loc-31)
  (= (road-length city-1-loc-53 city-1-loc-31) 16)
  ; 216,597 -> 160,449
  (road city-1-loc-31 city-1-loc-53)
  (= (road-length city-1-loc-31 city-1-loc-53) 16)
  ; 160,449 -> 262,415
  (road city-1-loc-53 city-1-loc-32)
  (= (road-length city-1-loc-53 city-1-loc-32) 11)
  ; 262,415 -> 160,449
  (road city-1-loc-32 city-1-loc-53)
  (= (road-length city-1-loc-32 city-1-loc-53) 11)
  ; 840,992 -> 765,898
  (road city-1-loc-54 city-1-loc-6)
  (= (road-length city-1-loc-54 city-1-loc-6) 12)
  ; 765,898 -> 840,992
  (road city-1-loc-6 city-1-loc-54)
  (= (road-length city-1-loc-6 city-1-loc-54) 12)
  ; 840,992 -> 922,875
  (road city-1-loc-54 city-1-loc-16)
  (= (road-length city-1-loc-54 city-1-loc-16) 15)
  ; 922,875 -> 840,992
  (road city-1-loc-16 city-1-loc-54)
  (= (road-length city-1-loc-16 city-1-loc-54) 15)
  ; 840,992 -> 997,974
  (road city-1-loc-54 city-1-loc-36)
  (= (road-length city-1-loc-54 city-1-loc-36) 16)
  ; 997,974 -> 840,992
  (road city-1-loc-36 city-1-loc-54)
  (= (road-length city-1-loc-36 city-1-loc-54) 16)
  ; 840,992 -> 681,982
  (road city-1-loc-54 city-1-loc-46)
  (= (road-length city-1-loc-54 city-1-loc-46) 16)
  ; 681,982 -> 840,992
  (road city-1-loc-46 city-1-loc-54)
  (= (road-length city-1-loc-46 city-1-loc-54) 16)
  ; 150,327 -> 55,393
  (road city-1-loc-55 city-1-loc-7)
  (= (road-length city-1-loc-55 city-1-loc-7) 12)
  ; 55,393 -> 150,327
  (road city-1-loc-7 city-1-loc-55)
  (= (road-length city-1-loc-7 city-1-loc-55) 12)
  ; 150,327 -> 13,248
  (road city-1-loc-55 city-1-loc-17)
  (= (road-length city-1-loc-55 city-1-loc-17) 16)
  ; 13,248 -> 150,327
  (road city-1-loc-17 city-1-loc-55)
  (= (road-length city-1-loc-17 city-1-loc-55) 16)
  ; 150,327 -> 148,205
  (road city-1-loc-55 city-1-loc-20)
  (= (road-length city-1-loc-55 city-1-loc-20) 13)
  ; 148,205 -> 150,327
  (road city-1-loc-20 city-1-loc-55)
  (= (road-length city-1-loc-20 city-1-loc-55) 13)
  ; 150,327 -> 267,298
  (road city-1-loc-55 city-1-loc-24)
  (= (road-length city-1-loc-55 city-1-loc-24) 13)
  ; 267,298 -> 150,327
  (road city-1-loc-24 city-1-loc-55)
  (= (road-length city-1-loc-24 city-1-loc-55) 13)
  ; 150,327 -> 262,415
  (road city-1-loc-55 city-1-loc-32)
  (= (road-length city-1-loc-55 city-1-loc-32) 15)
  ; 262,415 -> 150,327
  (road city-1-loc-32 city-1-loc-55)
  (= (road-length city-1-loc-32 city-1-loc-55) 15)
  ; 150,327 -> 160,449
  (road city-1-loc-55 city-1-loc-53)
  (= (road-length city-1-loc-55 city-1-loc-53) 13)
  ; 160,449 -> 150,327
  (road city-1-loc-53 city-1-loc-55)
  (= (road-length city-1-loc-53 city-1-loc-55) 13)
  ; 311,29 -> 425,51
  (road city-1-loc-56 city-1-loc-1)
  (= (road-length city-1-loc-56 city-1-loc-1) 12)
  ; 425,51 -> 311,29
  (road city-1-loc-1 city-1-loc-56)
  (= (road-length city-1-loc-1 city-1-loc-56) 12)
  ; 311,29 -> 276,182
  (road city-1-loc-56 city-1-loc-11)
  (= (road-length city-1-loc-56 city-1-loc-11) 16)
  ; 276,182 -> 311,29
  (road city-1-loc-11 city-1-loc-56)
  (= (road-length city-1-loc-11 city-1-loc-56) 16)
  ; 311,29 -> 206,48
  (road city-1-loc-56 city-1-loc-33)
  (= (road-length city-1-loc-56 city-1-loc-33) 11)
  ; 206,48 -> 311,29
  (road city-1-loc-33 city-1-loc-56)
  (= (road-length city-1-loc-33 city-1-loc-56) 11)
  ; 311,29 -> 385,168
  (road city-1-loc-56 city-1-loc-52)
  (= (road-length city-1-loc-56 city-1-loc-52) 16)
  ; 385,168 -> 311,29
  (road city-1-loc-52 city-1-loc-56)
  (= (road-length city-1-loc-52 city-1-loc-56) 16)
  ; 953,556 -> 944,697
  (road city-1-loc-57 city-1-loc-10)
  (= (road-length city-1-loc-57 city-1-loc-10) 15)
  ; 944,697 -> 953,556
  (road city-1-loc-10 city-1-loc-57)
  (= (road-length city-1-loc-10 city-1-loc-57) 15)
  ; 953,556 -> 835,584
  (road city-1-loc-57 city-1-loc-12)
  (= (road-length city-1-loc-57 city-1-loc-12) 13)
  ; 835,584 -> 953,556
  (road city-1-loc-12 city-1-loc-57)
  (= (road-length city-1-loc-12 city-1-loc-57) 13)
  ; 953,556 -> 987,459
  (road city-1-loc-57 city-1-loc-25)
  (= (road-length city-1-loc-57 city-1-loc-25) 11)
  ; 987,459 -> 953,556
  (road city-1-loc-25 city-1-loc-57)
  (= (road-length city-1-loc-25 city-1-loc-57) 11)
  ; 879,478 -> 755,388
  (road city-1-loc-58 city-1-loc-3)
  (= (road-length city-1-loc-58 city-1-loc-3) 16)
  ; 755,388 -> 879,478
  (road city-1-loc-3 city-1-loc-58)
  (= (road-length city-1-loc-3 city-1-loc-58) 16)
  ; 879,478 -> 835,584
  (road city-1-loc-58 city-1-loc-12)
  (= (road-length city-1-loc-58 city-1-loc-12) 12)
  ; 835,584 -> 879,478
  (road city-1-loc-12 city-1-loc-58)
  (= (road-length city-1-loc-12 city-1-loc-58) 12)
  ; 879,478 -> 987,459
  (road city-1-loc-58 city-1-loc-25)
  (= (road-length city-1-loc-58 city-1-loc-25) 11)
  ; 987,459 -> 879,478
  (road city-1-loc-25 city-1-loc-58)
  (= (road-length city-1-loc-25 city-1-loc-58) 11)
  ; 879,478 -> 907,360
  (road city-1-loc-58 city-1-loc-49)
  (= (road-length city-1-loc-58 city-1-loc-49) 13)
  ; 907,360 -> 879,478
  (road city-1-loc-49 city-1-loc-58)
  (= (road-length city-1-loc-49 city-1-loc-58) 13)
  ; 879,478 -> 953,556
  (road city-1-loc-58 city-1-loc-57)
  (= (road-length city-1-loc-58 city-1-loc-57) 11)
  ; 953,556 -> 879,478
  (road city-1-loc-57 city-1-loc-58)
  (= (road-length city-1-loc-57 city-1-loc-58) 11)
  ; 702,726 -> 700,557
  (road city-1-loc-59 city-1-loc-9)
  (= (road-length city-1-loc-59 city-1-loc-9) 17)
  ; 700,557 -> 702,726
  (road city-1-loc-9 city-1-loc-59)
  (= (road-length city-1-loc-9 city-1-loc-59) 17)
  ; 702,726 -> 625,642
  (road city-1-loc-59 city-1-loc-15)
  (= (road-length city-1-loc-59 city-1-loc-15) 12)
  ; 625,642 -> 702,726
  (road city-1-loc-15 city-1-loc-59)
  (= (road-length city-1-loc-15 city-1-loc-59) 12)
  ; 702,726 -> 626,869
  (road city-1-loc-59 city-1-loc-19)
  (= (road-length city-1-loc-59 city-1-loc-19) 17)
  ; 626,869 -> 702,726
  (road city-1-loc-19 city-1-loc-59)
  (= (road-length city-1-loc-19 city-1-loc-59) 17)
  ; 702,726 -> 815,689
  (road city-1-loc-59 city-1-loc-23)
  (= (road-length city-1-loc-59 city-1-loc-23) 12)
  ; 815,689 -> 702,726
  (road city-1-loc-23 city-1-loc-59)
  (= (road-length city-1-loc-23 city-1-loc-59) 12)
  ; 19,718 -> 81,825
  (road city-1-loc-60 city-1-loc-8)
  (= (road-length city-1-loc-60 city-1-loc-8) 13)
  ; 81,825 -> 19,718
  (road city-1-loc-8 city-1-loc-60)
  (= (road-length city-1-loc-8 city-1-loc-60) 13)
  ; 19,718 -> 126,655
  (road city-1-loc-60 city-1-loc-29)
  (= (road-length city-1-loc-60 city-1-loc-29) 13)
  ; 126,655 -> 19,718
  (road city-1-loc-29 city-1-loc-60)
  (= (road-length city-1-loc-29 city-1-loc-60) 13)
  ; 19,718 -> 22,566
  (road city-1-loc-60 city-1-loc-51)
  (= (road-length city-1-loc-60 city-1-loc-51) 16)
  ; 22,566 -> 19,718
  (road city-1-loc-51 city-1-loc-60)
  (= (road-length city-1-loc-51 city-1-loc-60) 16)
  ; 618,393 -> 755,388
  (road city-1-loc-61 city-1-loc-3)
  (= (road-length city-1-loc-61 city-1-loc-3) 14)
  ; 755,388 -> 618,393
  (road city-1-loc-3 city-1-loc-61)
  (= (road-length city-1-loc-3 city-1-loc-61) 14)
  ; 618,393 -> 512,409
  (road city-1-loc-61 city-1-loc-4)
  (= (road-length city-1-loc-61 city-1-loc-4) 11)
  ; 512,409 -> 618,393
  (road city-1-loc-4 city-1-loc-61)
  (= (road-length city-1-loc-4 city-1-loc-61) 11)
  ; 618,393 -> 557,286
  (road city-1-loc-61 city-1-loc-48)
  (= (road-length city-1-loc-61 city-1-loc-48) 13)
  ; 557,286 -> 618,393
  (road city-1-loc-48 city-1-loc-61)
  (= (road-length city-1-loc-48 city-1-loc-61) 13)
  ; 531,732 -> 554,565
  (road city-1-loc-62 city-1-loc-5)
  (= (road-length city-1-loc-62 city-1-loc-5) 17)
  ; 554,565 -> 531,732
  (road city-1-loc-5 city-1-loc-62)
  (= (road-length city-1-loc-5 city-1-loc-62) 17)
  ; 531,732 -> 389,798
  (road city-1-loc-62 city-1-loc-14)
  (= (road-length city-1-loc-62 city-1-loc-14) 16)
  ; 389,798 -> 531,732
  (road city-1-loc-14 city-1-loc-62)
  (= (road-length city-1-loc-14 city-1-loc-62) 16)
  ; 531,732 -> 625,642
  (road city-1-loc-62 city-1-loc-15)
  (= (road-length city-1-loc-62 city-1-loc-15) 13)
  ; 625,642 -> 531,732
  (road city-1-loc-15 city-1-loc-62)
  (= (road-length city-1-loc-15 city-1-loc-62) 13)
  ; 531,732 -> 626,869
  (road city-1-loc-62 city-1-loc-19)
  (= (road-length city-1-loc-62 city-1-loc-19) 17)
  ; 626,869 -> 531,732
  (road city-1-loc-19 city-1-loc-62)
  (= (road-length city-1-loc-19 city-1-loc-62) 17)
  ; 531,732 -> 527,842
  (road city-1-loc-62 city-1-loc-45)
  (= (road-length city-1-loc-62 city-1-loc-45) 11)
  ; 527,842 -> 531,732
  (road city-1-loc-45 city-1-loc-62)
  (= (road-length city-1-loc-45 city-1-loc-62) 11)
  ; 531,732 -> 702,726
  (road city-1-loc-62 city-1-loc-59)
  (= (road-length city-1-loc-62 city-1-loc-59) 18)
  ; 702,726 -> 531,732
  (road city-1-loc-59 city-1-loc-62)
  (= (road-length city-1-loc-59 city-1-loc-62) 18)
  ; 2339,66 -> 2185,122
  (road city-2-loc-7 city-2-loc-1)
  (= (road-length city-2-loc-7 city-2-loc-1) 17)
  ; 2185,122 -> 2339,66
  (road city-2-loc-1 city-2-loc-7)
  (= (road-length city-2-loc-1 city-2-loc-7) 17)
  ; 2938,653 -> 2991,496
  (road city-2-loc-11 city-2-loc-8)
  (= (road-length city-2-loc-11 city-2-loc-8) 17)
  ; 2991,496 -> 2938,653
  (road city-2-loc-8 city-2-loc-11)
  (= (road-length city-2-loc-8 city-2-loc-11) 17)
  ; 2385,181 -> 2539,130
  (road city-2-loc-14 city-2-loc-5)
  (= (road-length city-2-loc-14 city-2-loc-5) 17)
  ; 2539,130 -> 2385,181
  (road city-2-loc-5 city-2-loc-14)
  (= (road-length city-2-loc-5 city-2-loc-14) 17)
  ; 2385,181 -> 2334,284
  (road city-2-loc-14 city-2-loc-6)
  (= (road-length city-2-loc-14 city-2-loc-6) 12)
  ; 2334,284 -> 2385,181
  (road city-2-loc-6 city-2-loc-14)
  (= (road-length city-2-loc-6 city-2-loc-14) 12)
  ; 2385,181 -> 2339,66
  (road city-2-loc-14 city-2-loc-7)
  (= (road-length city-2-loc-14 city-2-loc-7) 13)
  ; 2339,66 -> 2385,181
  (road city-2-loc-7 city-2-loc-14)
  (= (road-length city-2-loc-7 city-2-loc-14) 13)
  ; 2782,695 -> 2938,653
  (road city-2-loc-17 city-2-loc-11)
  (= (road-length city-2-loc-17 city-2-loc-11) 17)
  ; 2938,653 -> 2782,695
  (road city-2-loc-11 city-2-loc-17)
  (= (road-length city-2-loc-11 city-2-loc-17) 17)
  ; 2435,557 -> 2563,484
  (road city-2-loc-18 city-2-loc-10)
  (= (road-length city-2-loc-18 city-2-loc-10) 15)
  ; 2563,484 -> 2435,557
  (road city-2-loc-10 city-2-loc-18)
  (= (road-length city-2-loc-10 city-2-loc-18) 15)
  ; 2435,557 -> 2555,658
  (road city-2-loc-18 city-2-loc-13)
  (= (road-length city-2-loc-18 city-2-loc-13) 16)
  ; 2555,658 -> 2435,557
  (road city-2-loc-13 city-2-loc-18)
  (= (road-length city-2-loc-13 city-2-loc-18) 16)
  ; 2435,557 -> 2337,657
  (road city-2-loc-18 city-2-loc-16)
  (= (road-length city-2-loc-18 city-2-loc-16) 14)
  ; 2337,657 -> 2435,557
  (road city-2-loc-16 city-2-loc-18)
  (= (road-length city-2-loc-16 city-2-loc-18) 14)
  ; 2228,247 -> 2185,122
  (road city-2-loc-19 city-2-loc-1)
  (= (road-length city-2-loc-19 city-2-loc-1) 14)
  ; 2185,122 -> 2228,247
  (road city-2-loc-1 city-2-loc-19)
  (= (road-length city-2-loc-1 city-2-loc-19) 14)
  ; 2228,247 -> 2114,308
  (road city-2-loc-19 city-2-loc-3)
  (= (road-length city-2-loc-19 city-2-loc-3) 13)
  ; 2114,308 -> 2228,247
  (road city-2-loc-3 city-2-loc-19)
  (= (road-length city-2-loc-3 city-2-loc-19) 13)
  ; 2228,247 -> 2334,284
  (road city-2-loc-19 city-2-loc-6)
  (= (road-length city-2-loc-19 city-2-loc-6) 12)
  ; 2334,284 -> 2228,247
  (road city-2-loc-6 city-2-loc-19)
  (= (road-length city-2-loc-6 city-2-loc-19) 12)
  ; 2228,247 -> 2385,181
  (road city-2-loc-19 city-2-loc-14)
  (= (road-length city-2-loc-19 city-2-loc-14) 17)
  ; 2385,181 -> 2228,247
  (road city-2-loc-14 city-2-loc-19)
  (= (road-length city-2-loc-14 city-2-loc-19) 17)
  ; 2312,545 -> 2337,657
  (road city-2-loc-20 city-2-loc-16)
  (= (road-length city-2-loc-20 city-2-loc-16) 12)
  ; 2337,657 -> 2312,545
  (road city-2-loc-16 city-2-loc-20)
  (= (road-length city-2-loc-16 city-2-loc-20) 12)
  ; 2312,545 -> 2435,557
  (road city-2-loc-20 city-2-loc-18)
  (= (road-length city-2-loc-20 city-2-loc-18) 13)
  ; 2435,557 -> 2312,545
  (road city-2-loc-18 city-2-loc-20)
  (= (road-length city-2-loc-18 city-2-loc-20) 13)
  ; 2144,999 -> 2260,872
  (road city-2-loc-21 city-2-loc-12)
  (= (road-length city-2-loc-21 city-2-loc-12) 18)
  ; 2260,872 -> 2144,999
  (road city-2-loc-12 city-2-loc-21)
  (= (road-length city-2-loc-12 city-2-loc-21) 18)
  ; 2080,439 -> 2114,308
  (road city-2-loc-22 city-2-loc-3)
  (= (road-length city-2-loc-22 city-2-loc-3) 14)
  ; 2114,308 -> 2080,439
  (road city-2-loc-3 city-2-loc-22)
  (= (road-length city-2-loc-3 city-2-loc-22) 14)
  ; 2080,439 -> 2046,566
  (road city-2-loc-22 city-2-loc-9)
  (= (road-length city-2-loc-22 city-2-loc-9) 14)
  ; 2046,566 -> 2080,439
  (road city-2-loc-9 city-2-loc-22)
  (= (road-length city-2-loc-9 city-2-loc-22) 14)
  ; 2753,556 -> 2782,695
  (road city-2-loc-23 city-2-loc-17)
  (= (road-length city-2-loc-23 city-2-loc-17) 15)
  ; 2782,695 -> 2753,556
  (road city-2-loc-17 city-2-loc-23)
  (= (road-length city-2-loc-17 city-2-loc-23) 15)
  ; 2165,545 -> 2046,566
  (road city-2-loc-25 city-2-loc-9)
  (= (road-length city-2-loc-25 city-2-loc-9) 13)
  ; 2046,566 -> 2165,545
  (road city-2-loc-9 city-2-loc-25)
  (= (road-length city-2-loc-9 city-2-loc-25) 13)
  ; 2165,545 -> 2312,545
  (road city-2-loc-25 city-2-loc-20)
  (= (road-length city-2-loc-25 city-2-loc-20) 15)
  ; 2312,545 -> 2165,545
  (road city-2-loc-20 city-2-loc-25)
  (= (road-length city-2-loc-20 city-2-loc-25) 15)
  ; 2165,545 -> 2080,439
  (road city-2-loc-25 city-2-loc-22)
  (= (road-length city-2-loc-25 city-2-loc-22) 14)
  ; 2080,439 -> 2165,545
  (road city-2-loc-22 city-2-loc-25)
  (= (road-length city-2-loc-22 city-2-loc-25) 14)
  ; 2469,720 -> 2555,658
  (road city-2-loc-27 city-2-loc-13)
  (= (road-length city-2-loc-27 city-2-loc-13) 11)
  ; 2555,658 -> 2469,720
  (road city-2-loc-13 city-2-loc-27)
  (= (road-length city-2-loc-13 city-2-loc-27) 11)
  ; 2469,720 -> 2337,657
  (road city-2-loc-27 city-2-loc-16)
  (= (road-length city-2-loc-27 city-2-loc-16) 15)
  ; 2337,657 -> 2469,720
  (road city-2-loc-16 city-2-loc-27)
  (= (road-length city-2-loc-16 city-2-loc-27) 15)
  ; 2469,720 -> 2435,557
  (road city-2-loc-27 city-2-loc-18)
  (= (road-length city-2-loc-27 city-2-loc-18) 17)
  ; 2435,557 -> 2469,720
  (road city-2-loc-18 city-2-loc-27)
  (= (road-length city-2-loc-18 city-2-loc-27) 17)
  ; 2469,720 -> 2534,857
  (road city-2-loc-27 city-2-loc-24)
  (= (road-length city-2-loc-27 city-2-loc-24) 16)
  ; 2534,857 -> 2469,720
  (road city-2-loc-24 city-2-loc-27)
  (= (road-length city-2-loc-24 city-2-loc-27) 16)
  ; 2507,993 -> 2534,857
  (road city-2-loc-28 city-2-loc-24)
  (= (road-length city-2-loc-28 city-2-loc-24) 14)
  ; 2534,857 -> 2507,993
  (road city-2-loc-24 city-2-loc-28)
  (= (road-length city-2-loc-24 city-2-loc-28) 14)
  ; 2002,228 -> 2114,308
  (road city-2-loc-29 city-2-loc-3)
  (= (road-length city-2-loc-29 city-2-loc-3) 14)
  ; 2114,308 -> 2002,228
  (road city-2-loc-3 city-2-loc-29)
  (= (road-length city-2-loc-3 city-2-loc-29) 14)
  ; 2674,115 -> 2539,130
  (road city-2-loc-30 city-2-loc-5)
  (= (road-length city-2-loc-30 city-2-loc-5) 14)
  ; 2539,130 -> 2674,115
  (road city-2-loc-5 city-2-loc-30)
  (= (road-length city-2-loc-5 city-2-loc-30) 14)
  ; 2183,740 -> 2260,872
  (road city-2-loc-31 city-2-loc-12)
  (= (road-length city-2-loc-31 city-2-loc-12) 16)
  ; 2260,872 -> 2183,740
  (road city-2-loc-12 city-2-loc-31)
  (= (road-length city-2-loc-12 city-2-loc-31) 16)
  ; 2991,361 -> 2861,363
  (road city-2-loc-33 city-2-loc-2)
  (= (road-length city-2-loc-33 city-2-loc-2) 13)
  ; 2861,363 -> 2991,361
  (road city-2-loc-2 city-2-loc-33)
  (= (road-length city-2-loc-2 city-2-loc-33) 13)
  ; 2991,361 -> 2991,496
  (road city-2-loc-33 city-2-loc-8)
  (= (road-length city-2-loc-33 city-2-loc-8) 14)
  ; 2991,496 -> 2991,361
  (road city-2-loc-8 city-2-loc-33)
  (= (road-length city-2-loc-8 city-2-loc-33) 14)
  ; 2305,407 -> 2334,284
  (road city-2-loc-34 city-2-loc-6)
  (= (road-length city-2-loc-34 city-2-loc-6) 13)
  ; 2334,284 -> 2305,407
  (road city-2-loc-6 city-2-loc-34)
  (= (road-length city-2-loc-6 city-2-loc-34) 13)
  ; 2305,407 -> 2312,545
  (road city-2-loc-34 city-2-loc-20)
  (= (road-length city-2-loc-34 city-2-loc-20) 14)
  ; 2312,545 -> 2305,407
  (road city-2-loc-20 city-2-loc-34)
  (= (road-length city-2-loc-20 city-2-loc-34) 14)
  ; 2667,714 -> 2555,658
  (road city-2-loc-35 city-2-loc-13)
  (= (road-length city-2-loc-35 city-2-loc-13) 13)
  ; 2555,658 -> 2667,714
  (road city-2-loc-13 city-2-loc-35)
  (= (road-length city-2-loc-13 city-2-loc-35) 13)
  ; 2667,714 -> 2782,695
  (road city-2-loc-35 city-2-loc-17)
  (= (road-length city-2-loc-35 city-2-loc-17) 12)
  ; 2782,695 -> 2667,714
  (road city-2-loc-17 city-2-loc-35)
  (= (road-length city-2-loc-17 city-2-loc-35) 12)
  ; 2987,236 -> 2862,167
  (road city-2-loc-36 city-2-loc-4)
  (= (road-length city-2-loc-36 city-2-loc-4) 15)
  ; 2862,167 -> 2987,236
  (road city-2-loc-4 city-2-loc-36)
  (= (road-length city-2-loc-4 city-2-loc-36) 15)
  ; 2987,236 -> 2991,361
  (road city-2-loc-36 city-2-loc-33)
  (= (road-length city-2-loc-36 city-2-loc-33) 13)
  ; 2991,361 -> 2987,236
  (road city-2-loc-33 city-2-loc-36)
  (= (road-length city-2-loc-33 city-2-loc-36) 13)
  ; 2761,224 -> 2861,363
  (road city-2-loc-37 city-2-loc-2)
  (= (road-length city-2-loc-37 city-2-loc-2) 18)
  ; 2861,363 -> 2761,224
  (road city-2-loc-2 city-2-loc-37)
  (= (road-length city-2-loc-2 city-2-loc-37) 18)
  ; 2761,224 -> 2862,167
  (road city-2-loc-37 city-2-loc-4)
  (= (road-length city-2-loc-37 city-2-loc-4) 12)
  ; 2862,167 -> 2761,224
  (road city-2-loc-4 city-2-loc-37)
  (= (road-length city-2-loc-4 city-2-loc-37) 12)
  ; 2761,224 -> 2674,115
  (road city-2-loc-37 city-2-loc-30)
  (= (road-length city-2-loc-37 city-2-loc-30) 14)
  ; 2674,115 -> 2761,224
  (road city-2-loc-30 city-2-loc-37)
  (= (road-length city-2-loc-30 city-2-loc-37) 14)
  ; 2761,224 -> 2647,326
  (road city-2-loc-37 city-2-loc-32)
  (= (road-length city-2-loc-37 city-2-loc-32) 16)
  ; 2647,326 -> 2761,224
  (road city-2-loc-32 city-2-loc-37)
  (= (road-length city-2-loc-32 city-2-loc-37) 16)
  ; 2829,16 -> 2862,167
  (road city-2-loc-38 city-2-loc-4)
  (= (road-length city-2-loc-38 city-2-loc-4) 16)
  ; 2862,167 -> 2829,16
  (road city-2-loc-4 city-2-loc-38)
  (= (road-length city-2-loc-4 city-2-loc-38) 16)
  ; 2500,333 -> 2563,484
  (road city-2-loc-39 city-2-loc-10)
  (= (road-length city-2-loc-39 city-2-loc-10) 17)
  ; 2563,484 -> 2500,333
  (road city-2-loc-10 city-2-loc-39)
  (= (road-length city-2-loc-10 city-2-loc-39) 17)
  ; 2500,333 -> 2647,326
  (road city-2-loc-39 city-2-loc-32)
  (= (road-length city-2-loc-39 city-2-loc-32) 15)
  ; 2647,326 -> 2500,333
  (road city-2-loc-32 city-2-loc-39)
  (= (road-length city-2-loc-32 city-2-loc-39) 15)
  ; 2989,991 -> 2928,843
  (road city-2-loc-40 city-2-loc-26)
  (= (road-length city-2-loc-40 city-2-loc-26) 16)
  ; 2928,843 -> 2989,991
  (road city-2-loc-26 city-2-loc-40)
  (= (road-length city-2-loc-26 city-2-loc-40) 16)
  ; 2568,761 -> 2555,658
  (road city-2-loc-41 city-2-loc-13)
  (= (road-length city-2-loc-41 city-2-loc-13) 11)
  ; 2555,658 -> 2568,761
  (road city-2-loc-13 city-2-loc-41)
  (= (road-length city-2-loc-13 city-2-loc-41) 11)
  ; 2568,761 -> 2534,857
  (road city-2-loc-41 city-2-loc-24)
  (= (road-length city-2-loc-41 city-2-loc-24) 11)
  ; 2534,857 -> 2568,761
  (road city-2-loc-24 city-2-loc-41)
  (= (road-length city-2-loc-24 city-2-loc-41) 11)
  ; 2568,761 -> 2469,720
  (road city-2-loc-41 city-2-loc-27)
  (= (road-length city-2-loc-41 city-2-loc-27) 11)
  ; 2469,720 -> 2568,761
  (road city-2-loc-27 city-2-loc-41)
  (= (road-length city-2-loc-27 city-2-loc-41) 11)
  ; 2568,761 -> 2667,714
  (road city-2-loc-41 city-2-loc-35)
  (= (road-length city-2-loc-41 city-2-loc-35) 11)
  ; 2667,714 -> 2568,761
  (road city-2-loc-35 city-2-loc-41)
  (= (road-length city-2-loc-35 city-2-loc-41) 11)
  ; 2655,523 -> 2563,484
  (road city-2-loc-42 city-2-loc-10)
  (= (road-length city-2-loc-42 city-2-loc-10) 10)
  ; 2563,484 -> 2655,523
  (road city-2-loc-10 city-2-loc-42)
  (= (road-length city-2-loc-10 city-2-loc-42) 10)
  ; 2655,523 -> 2555,658
  (road city-2-loc-42 city-2-loc-13)
  (= (road-length city-2-loc-42 city-2-loc-13) 17)
  ; 2555,658 -> 2655,523
  (road city-2-loc-13 city-2-loc-42)
  (= (road-length city-2-loc-13 city-2-loc-42) 17)
  ; 2655,523 -> 2753,556
  (road city-2-loc-42 city-2-loc-23)
  (= (road-length city-2-loc-42 city-2-loc-23) 11)
  ; 2753,556 -> 2655,523
  (road city-2-loc-23 city-2-loc-42)
  (= (road-length city-2-loc-23 city-2-loc-42) 11)
  ; 2448,81 -> 2539,130
  (road city-2-loc-43 city-2-loc-5)
  (= (road-length city-2-loc-43 city-2-loc-5) 11)
  ; 2539,130 -> 2448,81
  (road city-2-loc-5 city-2-loc-43)
  (= (road-length city-2-loc-5 city-2-loc-43) 11)
  ; 2448,81 -> 2339,66
  (road city-2-loc-43 city-2-loc-7)
  (= (road-length city-2-loc-43 city-2-loc-7) 11)
  ; 2339,66 -> 2448,81
  (road city-2-loc-7 city-2-loc-43)
  (= (road-length city-2-loc-7 city-2-loc-43) 11)
  ; 2448,81 -> 2385,181
  (road city-2-loc-43 city-2-loc-14)
  (= (road-length city-2-loc-43 city-2-loc-14) 12)
  ; 2385,181 -> 2448,81
  (road city-2-loc-14 city-2-loc-43)
  (= (road-length city-2-loc-14 city-2-loc-43) 12)
  ; 2983,18 -> 2829,16
  (road city-2-loc-44 city-2-loc-38)
  (= (road-length city-2-loc-44 city-2-loc-38) 16)
  ; 2829,16 -> 2983,18
  (road city-2-loc-38 city-2-loc-44)
  (= (road-length city-2-loc-38 city-2-loc-44) 16)
  ; 2628,216 -> 2539,130
  (road city-2-loc-46 city-2-loc-5)
  (= (road-length city-2-loc-46 city-2-loc-5) 13)
  ; 2539,130 -> 2628,216
  (road city-2-loc-5 city-2-loc-46)
  (= (road-length city-2-loc-5 city-2-loc-46) 13)
  ; 2628,216 -> 2674,115
  (road city-2-loc-46 city-2-loc-30)
  (= (road-length city-2-loc-46 city-2-loc-30) 12)
  ; 2674,115 -> 2628,216
  (road city-2-loc-30 city-2-loc-46)
  (= (road-length city-2-loc-30 city-2-loc-46) 12)
  ; 2628,216 -> 2647,326
  (road city-2-loc-46 city-2-loc-32)
  (= (road-length city-2-loc-46 city-2-loc-32) 12)
  ; 2647,326 -> 2628,216
  (road city-2-loc-32 city-2-loc-46)
  (= (road-length city-2-loc-32 city-2-loc-46) 12)
  ; 2628,216 -> 2761,224
  (road city-2-loc-46 city-2-loc-37)
  (= (road-length city-2-loc-46 city-2-loc-37) 14)
  ; 2761,224 -> 2628,216
  (road city-2-loc-37 city-2-loc-46)
  (= (road-length city-2-loc-37 city-2-loc-46) 14)
  ; 2128,862 -> 2260,872
  (road city-2-loc-47 city-2-loc-12)
  (= (road-length city-2-loc-47 city-2-loc-12) 14)
  ; 2260,872 -> 2128,862
  (road city-2-loc-12 city-2-loc-47)
  (= (road-length city-2-loc-12 city-2-loc-47) 14)
  ; 2128,862 -> 2027,842
  (road city-2-loc-47 city-2-loc-15)
  (= (road-length city-2-loc-47 city-2-loc-15) 11)
  ; 2027,842 -> 2128,862
  (road city-2-loc-15 city-2-loc-47)
  (= (road-length city-2-loc-15 city-2-loc-47) 11)
  ; 2128,862 -> 2144,999
  (road city-2-loc-47 city-2-loc-21)
  (= (road-length city-2-loc-47 city-2-loc-21) 14)
  ; 2144,999 -> 2128,862
  (road city-2-loc-21 city-2-loc-47)
  (= (road-length city-2-loc-21 city-2-loc-47) 14)
  ; 2128,862 -> 2183,740
  (road city-2-loc-47 city-2-loc-31)
  (= (road-length city-2-loc-47 city-2-loc-31) 14)
  ; 2183,740 -> 2128,862
  (road city-2-loc-31 city-2-loc-47)
  (= (road-length city-2-loc-31 city-2-loc-47) 14)
  ; 2853,600 -> 2938,653
  (road city-2-loc-48 city-2-loc-11)
  (= (road-length city-2-loc-48 city-2-loc-11) 10)
  ; 2938,653 -> 2853,600
  (road city-2-loc-11 city-2-loc-48)
  (= (road-length city-2-loc-11 city-2-loc-48) 10)
  ; 2853,600 -> 2782,695
  (road city-2-loc-48 city-2-loc-17)
  (= (road-length city-2-loc-48 city-2-loc-17) 12)
  ; 2782,695 -> 2853,600
  (road city-2-loc-17 city-2-loc-48)
  (= (road-length city-2-loc-17 city-2-loc-48) 12)
  ; 2853,600 -> 2753,556
  (road city-2-loc-48 city-2-loc-23)
  (= (road-length city-2-loc-48 city-2-loc-23) 11)
  ; 2753,556 -> 2853,600
  (road city-2-loc-23 city-2-loc-48)
  (= (road-length city-2-loc-23 city-2-loc-48) 11)
  ; 2038,941 -> 2027,842
  (road city-2-loc-49 city-2-loc-15)
  (= (road-length city-2-loc-49 city-2-loc-15) 10)
  ; 2027,842 -> 2038,941
  (road city-2-loc-15 city-2-loc-49)
  (= (road-length city-2-loc-15 city-2-loc-49) 10)
  ; 2038,941 -> 2144,999
  (road city-2-loc-49 city-2-loc-21)
  (= (road-length city-2-loc-49 city-2-loc-21) 13)
  ; 2144,999 -> 2038,941
  (road city-2-loc-21 city-2-loc-49)
  (= (road-length city-2-loc-21 city-2-loc-49) 13)
  ; 2038,941 -> 2128,862
  (road city-2-loc-49 city-2-loc-47)
  (= (road-length city-2-loc-49 city-2-loc-47) 12)
  ; 2128,862 -> 2038,941
  (road city-2-loc-47 city-2-loc-49)
  (= (road-length city-2-loc-47 city-2-loc-49) 12)
  ; 2257,990 -> 2260,872
  (road city-2-loc-50 city-2-loc-12)
  (= (road-length city-2-loc-50 city-2-loc-12) 12)
  ; 2260,872 -> 2257,990
  (road city-2-loc-12 city-2-loc-50)
  (= (road-length city-2-loc-12 city-2-loc-50) 12)
  ; 2257,990 -> 2144,999
  (road city-2-loc-50 city-2-loc-21)
  (= (road-length city-2-loc-50 city-2-loc-21) 12)
  ; 2144,999 -> 2257,990
  (road city-2-loc-21 city-2-loc-50)
  (= (road-length city-2-loc-21 city-2-loc-50) 12)
  ; 2030,690 -> 2046,566
  (road city-2-loc-51 city-2-loc-9)
  (= (road-length city-2-loc-51 city-2-loc-9) 13)
  ; 2046,566 -> 2030,690
  (road city-2-loc-9 city-2-loc-51)
  (= (road-length city-2-loc-9 city-2-loc-51) 13)
  ; 2030,690 -> 2027,842
  (road city-2-loc-51 city-2-loc-15)
  (= (road-length city-2-loc-51 city-2-loc-15) 16)
  ; 2027,842 -> 2030,690
  (road city-2-loc-15 city-2-loc-51)
  (= (road-length city-2-loc-15 city-2-loc-51) 16)
  ; 2030,690 -> 2183,740
  (road city-2-loc-51 city-2-loc-31)
  (= (road-length city-2-loc-51 city-2-loc-31) 17)
  ; 2183,740 -> 2030,690
  (road city-2-loc-31 city-2-loc-51)
  (= (road-length city-2-loc-31 city-2-loc-51) 17)
  ; 2813,916 -> 2928,843
  (road city-2-loc-52 city-2-loc-26)
  (= (road-length city-2-loc-52 city-2-loc-26) 14)
  ; 2928,843 -> 2813,916
  (road city-2-loc-26 city-2-loc-52)
  (= (road-length city-2-loc-26 city-2-loc-52) 14)
  ; 2813,916 -> 2681,949
  (road city-2-loc-52 city-2-loc-45)
  (= (road-length city-2-loc-52 city-2-loc-45) 14)
  ; 2681,949 -> 2813,916
  (road city-2-loc-45 city-2-loc-52)
  (= (road-length city-2-loc-45 city-2-loc-52) 14)
  ; 2717,804 -> 2782,695
  (road city-2-loc-53 city-2-loc-17)
  (= (road-length city-2-loc-53 city-2-loc-17) 13)
  ; 2782,695 -> 2717,804
  (road city-2-loc-17 city-2-loc-53)
  (= (road-length city-2-loc-17 city-2-loc-53) 13)
  ; 2717,804 -> 2667,714
  (road city-2-loc-53 city-2-loc-35)
  (= (road-length city-2-loc-53 city-2-loc-35) 11)
  ; 2667,714 -> 2717,804
  (road city-2-loc-35 city-2-loc-53)
  (= (road-length city-2-loc-35 city-2-loc-53) 11)
  ; 2717,804 -> 2568,761
  (road city-2-loc-53 city-2-loc-41)
  (= (road-length city-2-loc-53 city-2-loc-41) 16)
  ; 2568,761 -> 2717,804
  (road city-2-loc-41 city-2-loc-53)
  (= (road-length city-2-loc-41 city-2-loc-53) 16)
  ; 2717,804 -> 2681,949
  (road city-2-loc-53 city-2-loc-45)
  (= (road-length city-2-loc-53 city-2-loc-45) 15)
  ; 2681,949 -> 2717,804
  (road city-2-loc-45 city-2-loc-53)
  (= (road-length city-2-loc-45 city-2-loc-53) 15)
  ; 2717,804 -> 2813,916
  (road city-2-loc-53 city-2-loc-52)
  (= (road-length city-2-loc-53 city-2-loc-52) 15)
  ; 2813,916 -> 2717,804
  (road city-2-loc-52 city-2-loc-53)
  (= (road-length city-2-loc-52 city-2-loc-53) 15)
  ; 2123,12 -> 2185,122
  (road city-2-loc-54 city-2-loc-1)
  (= (road-length city-2-loc-54 city-2-loc-1) 13)
  ; 2185,122 -> 2123,12
  (road city-2-loc-1 city-2-loc-54)
  (= (road-length city-2-loc-1 city-2-loc-54) 13)
  ; 2955,122 -> 2862,167
  (road city-2-loc-55 city-2-loc-4)
  (= (road-length city-2-loc-55 city-2-loc-4) 11)
  ; 2862,167 -> 2955,122
  (road city-2-loc-4 city-2-loc-55)
  (= (road-length city-2-loc-4 city-2-loc-55) 11)
  ; 2955,122 -> 2987,236
  (road city-2-loc-55 city-2-loc-36)
  (= (road-length city-2-loc-55 city-2-loc-36) 12)
  ; 2987,236 -> 2955,122
  (road city-2-loc-36 city-2-loc-55)
  (= (road-length city-2-loc-36 city-2-loc-55) 12)
  ; 2955,122 -> 2829,16
  (road city-2-loc-55 city-2-loc-38)
  (= (road-length city-2-loc-55 city-2-loc-38) 17)
  ; 2829,16 -> 2955,122
  (road city-2-loc-38 city-2-loc-55)
  (= (road-length city-2-loc-38 city-2-loc-55) 17)
  ; 2955,122 -> 2983,18
  (road city-2-loc-55 city-2-loc-44)
  (= (road-length city-2-loc-55 city-2-loc-44) 11)
  ; 2983,18 -> 2955,122
  (road city-2-loc-44 city-2-loc-55)
  (= (road-length city-2-loc-44 city-2-loc-55) 11)
  ; 2138,643 -> 2046,566
  (road city-2-loc-56 city-2-loc-9)
  (= (road-length city-2-loc-56 city-2-loc-9) 12)
  ; 2046,566 -> 2138,643
  (road city-2-loc-9 city-2-loc-56)
  (= (road-length city-2-loc-9 city-2-loc-56) 12)
  ; 2138,643 -> 2165,545
  (road city-2-loc-56 city-2-loc-25)
  (= (road-length city-2-loc-56 city-2-loc-25) 11)
  ; 2165,545 -> 2138,643
  (road city-2-loc-25 city-2-loc-56)
  (= (road-length city-2-loc-25 city-2-loc-56) 11)
  ; 2138,643 -> 2183,740
  (road city-2-loc-56 city-2-loc-31)
  (= (road-length city-2-loc-56 city-2-loc-31) 11)
  ; 2183,740 -> 2138,643
  (road city-2-loc-31 city-2-loc-56)
  (= (road-length city-2-loc-31 city-2-loc-56) 11)
  ; 2138,643 -> 2030,690
  (road city-2-loc-56 city-2-loc-51)
  (= (road-length city-2-loc-56 city-2-loc-51) 12)
  ; 2030,690 -> 2138,643
  (road city-2-loc-51 city-2-loc-56)
  (= (road-length city-2-loc-51 city-2-loc-56) 12)
  ; 2798,442 -> 2861,363
  (road city-2-loc-57 city-2-loc-2)
  (= (road-length city-2-loc-57 city-2-loc-2) 11)
  ; 2861,363 -> 2798,442
  (road city-2-loc-2 city-2-loc-57)
  (= (road-length city-2-loc-2 city-2-loc-57) 11)
  ; 2798,442 -> 2753,556
  (road city-2-loc-57 city-2-loc-23)
  (= (road-length city-2-loc-57 city-2-loc-23) 13)
  ; 2753,556 -> 2798,442
  (road city-2-loc-23 city-2-loc-57)
  (= (road-length city-2-loc-23 city-2-loc-57) 13)
  ; 2798,442 -> 2655,523
  (road city-2-loc-57 city-2-loc-42)
  (= (road-length city-2-loc-57 city-2-loc-42) 17)
  ; 2655,523 -> 2798,442
  (road city-2-loc-42 city-2-loc-57)
  (= (road-length city-2-loc-42 city-2-loc-57) 17)
  ; 2798,442 -> 2853,600
  (road city-2-loc-57 city-2-loc-48)
  (= (road-length city-2-loc-57 city-2-loc-48) 17)
  ; 2853,600 -> 2798,442
  (road city-2-loc-48 city-2-loc-57)
  (= (road-length city-2-loc-48 city-2-loc-57) 17)
  ; 2871,747 -> 2938,653
  (road city-2-loc-58 city-2-loc-11)
  (= (road-length city-2-loc-58 city-2-loc-11) 12)
  ; 2938,653 -> 2871,747
  (road city-2-loc-11 city-2-loc-58)
  (= (road-length city-2-loc-11 city-2-loc-58) 12)
  ; 2871,747 -> 2782,695
  (road city-2-loc-58 city-2-loc-17)
  (= (road-length city-2-loc-58 city-2-loc-17) 11)
  ; 2782,695 -> 2871,747
  (road city-2-loc-17 city-2-loc-58)
  (= (road-length city-2-loc-17 city-2-loc-58) 11)
  ; 2871,747 -> 2928,843
  (road city-2-loc-58 city-2-loc-26)
  (= (road-length city-2-loc-58 city-2-loc-26) 12)
  ; 2928,843 -> 2871,747
  (road city-2-loc-26 city-2-loc-58)
  (= (road-length city-2-loc-26 city-2-loc-58) 12)
  ; 2871,747 -> 2853,600
  (road city-2-loc-58 city-2-loc-48)
  (= (road-length city-2-loc-58 city-2-loc-48) 15)
  ; 2853,600 -> 2871,747
  (road city-2-loc-48 city-2-loc-58)
  (= (road-length city-2-loc-48 city-2-loc-58) 15)
  ; 2871,747 -> 2717,804
  (road city-2-loc-58 city-2-loc-53)
  (= (road-length city-2-loc-58 city-2-loc-53) 17)
  ; 2717,804 -> 2871,747
  (road city-2-loc-53 city-2-loc-58)
  (= (road-length city-2-loc-53 city-2-loc-58) 17)
  ; 2369,934 -> 2260,872
  (road city-2-loc-59 city-2-loc-12)
  (= (road-length city-2-loc-59 city-2-loc-12) 13)
  ; 2260,872 -> 2369,934
  (road city-2-loc-12 city-2-loc-59)
  (= (road-length city-2-loc-12 city-2-loc-59) 13)
  ; 2369,934 -> 2507,993
  (road city-2-loc-59 city-2-loc-28)
  (= (road-length city-2-loc-59 city-2-loc-28) 15)
  ; 2507,993 -> 2369,934
  (road city-2-loc-28 city-2-loc-59)
  (= (road-length city-2-loc-28 city-2-loc-59) 15)
  ; 2369,934 -> 2257,990
  (road city-2-loc-59 city-2-loc-50)
  (= (road-length city-2-loc-59 city-2-loc-50) 13)
  ; 2257,990 -> 2369,934
  (road city-2-loc-50 city-2-loc-59)
  (= (road-length city-2-loc-50 city-2-loc-59) 13)
  ; 2010,8 -> 2123,12
  (road city-2-loc-60 city-2-loc-54)
  (= (road-length city-2-loc-60 city-2-loc-54) 12)
  ; 2123,12 -> 2010,8
  (road city-2-loc-54 city-2-loc-60)
  (= (road-length city-2-loc-54 city-2-loc-60) 12)
  ; 2180,432 -> 2114,308
  (road city-2-loc-61 city-2-loc-3)
  (= (road-length city-2-loc-61 city-2-loc-3) 14)
  ; 2114,308 -> 2180,432
  (road city-2-loc-3 city-2-loc-61)
  (= (road-length city-2-loc-3 city-2-loc-61) 14)
  ; 2180,432 -> 2080,439
  (road city-2-loc-61 city-2-loc-22)
  (= (road-length city-2-loc-61 city-2-loc-22) 10)
  ; 2080,439 -> 2180,432
  (road city-2-loc-22 city-2-loc-61)
  (= (road-length city-2-loc-22 city-2-loc-61) 10)
  ; 2180,432 -> 2165,545
  (road city-2-loc-61 city-2-loc-25)
  (= (road-length city-2-loc-61 city-2-loc-25) 12)
  ; 2165,545 -> 2180,432
  (road city-2-loc-25 city-2-loc-61)
  (= (road-length city-2-loc-25 city-2-loc-61) 12)
  ; 2180,432 -> 2305,407
  (road city-2-loc-61 city-2-loc-34)
  (= (road-length city-2-loc-61 city-2-loc-34) 13)
  ; 2305,407 -> 2180,432
  (road city-2-loc-34 city-2-loc-61)
  (= (road-length city-2-loc-34 city-2-loc-61) 13)
  ; 2325,790 -> 2260,872
  (road city-2-loc-62 city-2-loc-12)
  (= (road-length city-2-loc-62 city-2-loc-12) 11)
  ; 2260,872 -> 2325,790
  (road city-2-loc-12 city-2-loc-62)
  (= (road-length city-2-loc-12 city-2-loc-62) 11)
  ; 2325,790 -> 2337,657
  (road city-2-loc-62 city-2-loc-16)
  (= (road-length city-2-loc-62 city-2-loc-16) 14)
  ; 2337,657 -> 2325,790
  (road city-2-loc-16 city-2-loc-62)
  (= (road-length city-2-loc-16 city-2-loc-62) 14)
  ; 2325,790 -> 2469,720
  (road city-2-loc-62 city-2-loc-27)
  (= (road-length city-2-loc-62 city-2-loc-27) 16)
  ; 2469,720 -> 2325,790
  (road city-2-loc-27 city-2-loc-62)
  (= (road-length city-2-loc-27 city-2-loc-62) 16)
  ; 2325,790 -> 2183,740
  (road city-2-loc-62 city-2-loc-31)
  (= (road-length city-2-loc-62 city-2-loc-31) 16)
  ; 2183,740 -> 2325,790
  (road city-2-loc-31 city-2-loc-62)
  (= (road-length city-2-loc-31 city-2-loc-62) 16)
  ; 2325,790 -> 2369,934
  (road city-2-loc-62 city-2-loc-59)
  (= (road-length city-2-loc-62 city-2-loc-59) 16)
  ; 2369,934 -> 2325,790
  (road city-2-loc-59 city-2-loc-62)
  (= (road-length city-2-loc-59 city-2-loc-62) 16)
  ; 968,139 <-> 2002,228
  (road city-1-loc-39 city-2-loc-29)
  (= (road-length city-1-loc-39 city-2-loc-29) 104)
  (road city-2-loc-29 city-1-loc-39)
  (= (road-length city-2-loc-29 city-1-loc-39) 104)
  (at package-1 city-1-loc-11)
  (at package-2 city-1-loc-54)
  (at package-3 city-1-loc-25)
  (at package-4 city-1-loc-40)
  (at package-5 city-1-loc-13)
  (at package-6 city-1-loc-49)
  (at package-7 city-1-loc-42)
  (at package-8 city-1-loc-38)
  (at package-9 city-1-loc-51)
  (at package-10 city-1-loc-36)
  (at package-11 city-1-loc-43)
  (at package-12 city-1-loc-4)
  (at package-13 city-1-loc-54)
  (at package-14 city-1-loc-54)
  (at package-15 city-1-loc-4)
  (at package-16 city-1-loc-52)
  (at package-17 city-1-loc-15)
  (at package-18 city-1-loc-34)
  (at package-19 city-1-loc-26)
  (at package-20 city-1-loc-5)
  (at package-21 city-1-loc-62)
  (at package-22 city-1-loc-40)
  (at truck-1 city-2-loc-21)
  (capacity truck-1 capacity-3)
  (at truck-2 city-2-loc-47)
  (capacity truck-2 capacity-4)
  (at truck-3 city-2-loc-62)
  (capacity truck-3 capacity-2)
  (at truck-4 city-2-loc-6)
  (capacity truck-4 capacity-4)
 )
 (:goal (and
  (at package-1 city-2-loc-58)
  (at package-2 city-2-loc-17)
  (at package-3 city-2-loc-29)
  (at package-4 city-2-loc-30)
  (at package-5 city-2-loc-49)
  (at package-6 city-2-loc-11)
  (at package-7 city-2-loc-49)
  (at package-8 city-2-loc-39)
  (at package-9 city-2-loc-33)
  (at package-10 city-2-loc-61)
  (at package-11 city-2-loc-53)
  (at package-12 city-2-loc-44)
  (at package-13 city-2-loc-21)
  (at package-14 city-2-loc-36)
  (at package-15 city-2-loc-61)
  (at package-16 city-2-loc-9)
  (at package-17 city-2-loc-40)
  (at package-18 city-2-loc-10)
  (at package-19 city-2-loc-57)
  (at package-20 city-2-loc-34)
  (at package-21 city-2-loc-51)
  (at package-22 city-2-loc-51)
 ))
 (:metric minimize (total-cost))
)
