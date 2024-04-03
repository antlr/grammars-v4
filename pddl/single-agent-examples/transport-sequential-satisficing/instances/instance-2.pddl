; Transport city-sequential-45nodes-1000size-4degree-100mindistance-4trucks-18packages-2008seed

(define (problem transport-city-sequential-45nodes-1000size-4degree-100mindistance-4trucks-18packages-2008seed)
 (:domain transport)
 (:objects
  city-loc-1 - location
  city-loc-2 - location
  city-loc-3 - location
  city-loc-4 - location
  city-loc-5 - location
  city-loc-6 - location
  city-loc-7 - location
  city-loc-8 - location
  city-loc-9 - location
  city-loc-10 - location
  city-loc-11 - location
  city-loc-12 - location
  city-loc-13 - location
  city-loc-14 - location
  city-loc-15 - location
  city-loc-16 - location
  city-loc-17 - location
  city-loc-18 - location
  city-loc-19 - location
  city-loc-20 - location
  city-loc-21 - location
  city-loc-22 - location
  city-loc-23 - location
  city-loc-24 - location
  city-loc-25 - location
  city-loc-26 - location
  city-loc-27 - location
  city-loc-28 - location
  city-loc-29 - location
  city-loc-30 - location
  city-loc-31 - location
  city-loc-32 - location
  city-loc-33 - location
  city-loc-34 - location
  city-loc-35 - location
  city-loc-36 - location
  city-loc-37 - location
  city-loc-38 - location
  city-loc-39 - location
  city-loc-40 - location
  city-loc-41 - location
  city-loc-42 - location
  city-loc-43 - location
  city-loc-44 - location
  city-loc-45 - location
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
  ; 977,899 -> 912,799
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 12)
  ; 912,799 -> 977,899
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 12)
  ; 456,221 -> 384,50
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 19)
  ; 384,50 -> 456,221
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 19)
  ; 742,542 -> 890,543
  (road city-loc-7 city-loc-1)
  (= (road-length city-loc-7 city-loc-1) 15)
  ; 890,543 -> 742,542
  (road city-loc-1 city-loc-7)
  (= (road-length city-loc-1 city-loc-7) 15)
  ; 742,542 -> 748,385
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 16)
  ; 748,385 -> 742,542
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 16)
  ; 566,552 -> 742,542
  (road city-loc-10 city-loc-7)
  (= (road-length city-loc-10 city-loc-7) 18)
  ; 742,542 -> 566,552
  (road city-loc-7 city-loc-10)
  (= (road-length city-loc-7 city-loc-10) 18)
  ; 55,605 -> 174,643
  (road city-loc-13 city-loc-11)
  (= (road-length city-loc-13 city-loc-11) 13)
  ; 174,643 -> 55,605
  (road city-loc-11 city-loc-13)
  (= (road-length city-loc-11 city-loc-13) 13)
  ; 803,858 -> 912,799
  (road city-loc-14 city-loc-4)
  (= (road-length city-loc-14 city-loc-4) 13)
  ; 912,799 -> 803,858
  (road city-loc-4 city-loc-14)
  (= (road-length city-loc-4 city-loc-14) 13)
  ; 803,858 -> 977,899
  (road city-loc-14 city-loc-5)
  (= (road-length city-loc-14 city-loc-5) 18)
  ; 977,899 -> 803,858
  (road city-loc-5 city-loc-14)
  (= (road-length city-loc-5 city-loc-14) 18)
  ; 263,567 -> 273,425
  (road city-loc-15 city-loc-9)
  (= (road-length city-loc-15 city-loc-9) 15)
  ; 273,425 -> 263,567
  (road city-loc-9 city-loc-15)
  (= (road-length city-loc-9 city-loc-15) 15)
  ; 263,567 -> 174,643
  (road city-loc-15 city-loc-11)
  (= (road-length city-loc-15 city-loc-11) 12)
  ; 174,643 -> 263,567
  (road city-loc-11 city-loc-15)
  (= (road-length city-loc-11 city-loc-15) 12)
  ; 128,791 -> 174,643
  (road city-loc-16 city-loc-11)
  (= (road-length city-loc-16 city-loc-11) 16)
  ; 174,643 -> 128,791
  (road city-loc-11 city-loc-16)
  (= (road-length city-loc-11 city-loc-16) 16)
  ; 128,791 -> 55,605
  (road city-loc-16 city-loc-13)
  (= (road-length city-loc-16 city-loc-13) 20)
  ; 55,605 -> 128,791
  (road city-loc-13 city-loc-16)
  (= (road-length city-loc-13 city-loc-16) 20)
  ; 426,706 -> 564,783
  (road city-loc-17 city-loc-8)
  (= (road-length city-loc-17 city-loc-8) 16)
  ; 564,783 -> 426,706
  (road city-loc-8 city-loc-17)
  (= (road-length city-loc-8 city-loc-17) 16)
  ; 392,433 -> 273,425
  (road city-loc-21 city-loc-9)
  (= (road-length city-loc-21 city-loc-9) 12)
  ; 273,425 -> 392,433
  (road city-loc-9 city-loc-21)
  (= (road-length city-loc-9 city-loc-21) 12)
  ; 392,433 -> 263,567
  (road city-loc-21 city-loc-15)
  (= (road-length city-loc-21 city-loc-15) 19)
  ; 263,567 -> 392,433
  (road city-loc-15 city-loc-21)
  (= (road-length city-loc-15 city-loc-21) 19)
  ; 231,881 -> 128,791
  (road city-loc-22 city-loc-16)
  (= (road-length city-loc-22 city-loc-16) 14)
  ; 128,791 -> 231,881
  (road city-loc-16 city-loc-22)
  (= (road-length city-loc-16 city-loc-22) 14)
  ; 682,8 -> 806,18
  (road city-loc-23 city-loc-19)
  (= (road-length city-loc-23 city-loc-19) 13)
  ; 806,18 -> 682,8
  (road city-loc-19 city-loc-23)
  (= (road-length city-loc-19 city-loc-23) 13)
  ; 989,457 -> 890,543
  (road city-loc-24 city-loc-1)
  (= (road-length city-loc-24 city-loc-1) 14)
  ; 890,543 -> 989,457
  (road city-loc-1 city-loc-24)
  (= (road-length city-loc-1 city-loc-24) 14)
  ; 362,862 -> 426,706
  (road city-loc-25 city-loc-17)
  (= (road-length city-loc-25 city-loc-17) 17)
  ; 426,706 -> 362,862
  (road city-loc-17 city-loc-25)
  (= (road-length city-loc-17 city-loc-25) 17)
  ; 362,862 -> 231,881
  (road city-loc-25 city-loc-22)
  (= (road-length city-loc-25 city-loc-22) 14)
  ; 231,881 -> 362,862
  (road city-loc-22 city-loc-25)
  (= (road-length city-loc-22 city-loc-25) 14)
  ; 6,60 -> 138,109
  (road city-loc-26 city-loc-20)
  (= (road-length city-loc-26 city-loc-20) 15)
  ; 138,109 -> 6,60
  (road city-loc-20 city-loc-26)
  (= (road-length city-loc-20 city-loc-26) 15)
  ; 257,5 -> 384,50
  (road city-loc-27 city-loc-2)
  (= (road-length city-loc-27 city-loc-2) 14)
  ; 384,50 -> 257,5
  (road city-loc-2 city-loc-27)
  (= (road-length city-loc-2 city-loc-27) 14)
  ; 257,5 -> 138,109
  (road city-loc-27 city-loc-20)
  (= (road-length city-loc-27 city-loc-20) 16)
  ; 138,109 -> 257,5
  (road city-loc-20 city-loc-27)
  (= (road-length city-loc-20 city-loc-27) 16)
  ; 347,149 -> 384,50
  (road city-loc-28 city-loc-2)
  (= (road-length city-loc-28 city-loc-2) 11)
  ; 384,50 -> 347,149
  (road city-loc-2 city-loc-28)
  (= (road-length city-loc-2 city-loc-28) 11)
  ; 347,149 -> 456,221
  (road city-loc-28 city-loc-6)
  (= (road-length city-loc-28 city-loc-6) 14)
  ; 456,221 -> 347,149
  (road city-loc-6 city-loc-28)
  (= (road-length city-loc-6 city-loc-28) 14)
  ; 347,149 -> 257,5
  (road city-loc-28 city-loc-27)
  (= (road-length city-loc-28 city-loc-27) 17)
  ; 257,5 -> 347,149
  (road city-loc-27 city-loc-28)
  (= (road-length city-loc-27 city-loc-28) 17)
  ; 521,375 -> 456,221
  (road city-loc-29 city-loc-6)
  (= (road-length city-loc-29 city-loc-6) 17)
  ; 456,221 -> 521,375
  (road city-loc-6 city-loc-29)
  (= (road-length city-loc-6 city-loc-29) 17)
  ; 521,375 -> 566,552
  (road city-loc-29 city-loc-10)
  (= (road-length city-loc-29 city-loc-10) 19)
  ; 566,552 -> 521,375
  (road city-loc-10 city-loc-29)
  (= (road-length city-loc-10 city-loc-29) 19)
  ; 521,375 -> 392,433
  (road city-loc-29 city-loc-21)
  (= (road-length city-loc-29 city-loc-21) 15)
  ; 392,433 -> 521,375
  (road city-loc-21 city-loc-29)
  (= (road-length city-loc-21 city-loc-29) 15)
  ; 720,241 -> 748,385
  (road city-loc-30 city-loc-3)
  (= (road-length city-loc-30 city-loc-3) 15)
  ; 748,385 -> 720,241
  (road city-loc-3 city-loc-30)
  (= (road-length city-loc-3 city-loc-30) 15)
  ; 377,283 -> 456,221
  (road city-loc-31 city-loc-6)
  (= (road-length city-loc-31 city-loc-6) 10)
  ; 456,221 -> 377,283
  (road city-loc-6 city-loc-31)
  (= (road-length city-loc-6 city-loc-31) 10)
  ; 377,283 -> 273,425
  (road city-loc-31 city-loc-9)
  (= (road-length city-loc-31 city-loc-9) 18)
  ; 273,425 -> 377,283
  (road city-loc-9 city-loc-31)
  (= (road-length city-loc-9 city-loc-31) 18)
  ; 377,283 -> 392,433
  (road city-loc-31 city-loc-21)
  (= (road-length city-loc-31 city-loc-21) 16)
  ; 392,433 -> 377,283
  (road city-loc-21 city-loc-31)
  (= (road-length city-loc-21 city-loc-31) 16)
  ; 377,283 -> 347,149
  (road city-loc-31 city-loc-28)
  (= (road-length city-loc-31 city-loc-28) 14)
  ; 347,149 -> 377,283
  (road city-loc-28 city-loc-31)
  (= (road-length city-loc-28 city-loc-31) 14)
  ; 377,283 -> 521,375
  (road city-loc-31 city-loc-29)
  (= (road-length city-loc-31 city-loc-29) 18)
  ; 521,375 -> 377,283
  (road city-loc-29 city-loc-31)
  (= (road-length city-loc-29 city-loc-31) 18)
  ; 643,669 -> 742,542
  (road city-loc-32 city-loc-7)
  (= (road-length city-loc-32 city-loc-7) 17)
  ; 742,542 -> 643,669
  (road city-loc-7 city-loc-32)
  (= (road-length city-loc-7 city-loc-32) 17)
  ; 643,669 -> 564,783
  (road city-loc-32 city-loc-8)
  (= (road-length city-loc-32 city-loc-8) 14)
  ; 564,783 -> 643,669
  (road city-loc-8 city-loc-32)
  (= (road-length city-loc-8 city-loc-32) 14)
  ; 643,669 -> 566,552
  (road city-loc-32 city-loc-10)
  (= (road-length city-loc-32 city-loc-10) 14)
  ; 566,552 -> 643,669
  (road city-loc-10 city-loc-32)
  (= (road-length city-loc-10 city-loc-32) 14)
  ; 858,139 -> 930,259
  (road city-loc-33 city-loc-12)
  (= (road-length city-loc-33 city-loc-12) 14)
  ; 930,259 -> 858,139
  (road city-loc-12 city-loc-33)
  (= (road-length city-loc-12 city-loc-33) 14)
  ; 858,139 -> 806,18
  (road city-loc-33 city-loc-19)
  (= (road-length city-loc-33 city-loc-19) 14)
  ; 806,18 -> 858,139
  (road city-loc-19 city-loc-33)
  (= (road-length city-loc-19 city-loc-33) 14)
  ; 858,139 -> 720,241
  (road city-loc-33 city-loc-30)
  (= (road-length city-loc-33 city-loc-30) 18)
  ; 720,241 -> 858,139
  (road city-loc-30 city-loc-33)
  (= (road-length city-loc-30 city-loc-33) 18)
  ; 203,987 -> 231,881
  (road city-loc-34 city-loc-22)
  (= (road-length city-loc-34 city-loc-22) 11)
  ; 231,881 -> 203,987
  (road city-loc-22 city-loc-34)
  (= (road-length city-loc-22 city-loc-34) 11)
  ; 560,901 -> 564,783
  (road city-loc-35 city-loc-8)
  (= (road-length city-loc-35 city-loc-8) 12)
  ; 564,783 -> 560,901
  (road city-loc-8 city-loc-35)
  (= (road-length city-loc-8 city-loc-35) 12)
  ; 437,605 -> 566,552
  (road city-loc-36 city-loc-10)
  (= (road-length city-loc-36 city-loc-10) 14)
  ; 566,552 -> 437,605
  (road city-loc-10 city-loc-36)
  (= (road-length city-loc-10 city-loc-36) 14)
  ; 437,605 -> 263,567
  (road city-loc-36 city-loc-15)
  (= (road-length city-loc-36 city-loc-15) 18)
  ; 263,567 -> 437,605
  (road city-loc-15 city-loc-36)
  (= (road-length city-loc-15 city-loc-36) 18)
  ; 437,605 -> 426,706
  (road city-loc-36 city-loc-17)
  (= (road-length city-loc-36 city-loc-17) 11)
  ; 426,706 -> 437,605
  (road city-loc-17 city-loc-36)
  (= (road-length city-loc-17 city-loc-36) 11)
  ; 437,605 -> 392,433
  (road city-loc-36 city-loc-21)
  (= (road-length city-loc-36 city-loc-21) 18)
  ; 392,433 -> 437,605
  (road city-loc-21 city-loc-36)
  (= (road-length city-loc-21 city-loc-36) 18)
  ; 806,647 -> 890,543
  (road city-loc-37 city-loc-1)
  (= (road-length city-loc-37 city-loc-1) 14)
  ; 890,543 -> 806,647
  (road city-loc-1 city-loc-37)
  (= (road-length city-loc-1 city-loc-37) 14)
  ; 806,647 -> 912,799
  (road city-loc-37 city-loc-4)
  (= (road-length city-loc-37 city-loc-4) 19)
  ; 912,799 -> 806,647
  (road city-loc-4 city-loc-37)
  (= (road-length city-loc-4 city-loc-37) 19)
  ; 806,647 -> 742,542
  (road city-loc-37 city-loc-7)
  (= (road-length city-loc-37 city-loc-7) 13)
  ; 742,542 -> 806,647
  (road city-loc-7 city-loc-37)
  (= (road-length city-loc-7 city-loc-37) 13)
  ; 806,647 -> 643,669
  (road city-loc-37 city-loc-32)
  (= (road-length city-loc-37 city-loc-32) 17)
  ; 643,669 -> 806,647
  (road city-loc-32 city-loc-37)
  (= (road-length city-loc-32 city-loc-37) 17)
  ; 339,962 -> 231,881
  (road city-loc-38 city-loc-22)
  (= (road-length city-loc-38 city-loc-22) 14)
  ; 231,881 -> 339,962
  (road city-loc-22 city-loc-38)
  (= (road-length city-loc-22 city-loc-38) 14)
  ; 339,962 -> 362,862
  (road city-loc-38 city-loc-25)
  (= (road-length city-loc-38 city-loc-25) 11)
  ; 362,862 -> 339,962
  (road city-loc-25 city-loc-38)
  (= (road-length city-loc-25 city-loc-38) 11)
  ; 339,962 -> 203,987
  (road city-loc-38 city-loc-34)
  (= (road-length city-loc-38 city-loc-34) 14)
  ; 203,987 -> 339,962
  (road city-loc-34 city-loc-38)
  (= (road-length city-loc-34 city-loc-38) 14)
  ; 463,927 -> 564,783
  (road city-loc-39 city-loc-8)
  (= (road-length city-loc-39 city-loc-8) 18)
  ; 564,783 -> 463,927
  (road city-loc-8 city-loc-39)
  (= (road-length city-loc-8 city-loc-39) 18)
  ; 463,927 -> 362,862
  (road city-loc-39 city-loc-25)
  (= (road-length city-loc-39 city-loc-25) 12)
  ; 362,862 -> 463,927
  (road city-loc-25 city-loc-39)
  (= (road-length city-loc-25 city-loc-39) 12)
  ; 463,927 -> 560,901
  (road city-loc-39 city-loc-35)
  (= (road-length city-loc-39 city-loc-35) 10)
  ; 560,901 -> 463,927
  (road city-loc-35 city-loc-39)
  (= (road-length city-loc-35 city-loc-39) 10)
  ; 463,927 -> 339,962
  (road city-loc-39 city-loc-38)
  (= (road-length city-loc-39 city-loc-38) 13)
  ; 339,962 -> 463,927
  (road city-loc-38 city-loc-39)
  (= (road-length city-loc-38 city-loc-39) 13)
  ; 281,709 -> 174,643
  (road city-loc-40 city-loc-11)
  (= (road-length city-loc-40 city-loc-11) 13)
  ; 174,643 -> 281,709
  (road city-loc-11 city-loc-40)
  (= (road-length city-loc-11 city-loc-40) 13)
  ; 281,709 -> 263,567
  (road city-loc-40 city-loc-15)
  (= (road-length city-loc-40 city-loc-15) 15)
  ; 263,567 -> 281,709
  (road city-loc-15 city-loc-40)
  (= (road-length city-loc-15 city-loc-40) 15)
  ; 281,709 -> 128,791
  (road city-loc-40 city-loc-16)
  (= (road-length city-loc-40 city-loc-16) 18)
  ; 128,791 -> 281,709
  (road city-loc-16 city-loc-40)
  (= (road-length city-loc-16 city-loc-40) 18)
  ; 281,709 -> 426,706
  (road city-loc-40 city-loc-17)
  (= (road-length city-loc-40 city-loc-17) 15)
  ; 426,706 -> 281,709
  (road city-loc-17 city-loc-40)
  (= (road-length city-loc-17 city-loc-40) 15)
  ; 281,709 -> 231,881
  (road city-loc-40 city-loc-22)
  (= (road-length city-loc-40 city-loc-22) 18)
  ; 231,881 -> 281,709
  (road city-loc-22 city-loc-40)
  (= (road-length city-loc-22 city-loc-40) 18)
  ; 281,709 -> 362,862
  (road city-loc-40 city-loc-25)
  (= (road-length city-loc-40 city-loc-25) 18)
  ; 362,862 -> 281,709
  (road city-loc-25 city-loc-40)
  (= (road-length city-loc-25 city-loc-40) 18)
  ; 281,709 -> 437,605
  (road city-loc-40 city-loc-36)
  (= (road-length city-loc-40 city-loc-36) 19)
  ; 437,605 -> 281,709
  (road city-loc-36 city-loc-40)
  (= (road-length city-loc-36 city-loc-40) 19)
  ; 205,275 -> 273,425
  (road city-loc-41 city-loc-9)
  (= (road-length city-loc-41 city-loc-9) 17)
  ; 273,425 -> 205,275
  (road city-loc-9 city-loc-41)
  (= (road-length city-loc-9 city-loc-41) 17)
  ; 205,275 -> 36,368
  (road city-loc-41 city-loc-18)
  (= (road-length city-loc-41 city-loc-18) 20)
  ; 36,368 -> 205,275
  (road city-loc-18 city-loc-41)
  (= (road-length city-loc-18 city-loc-41) 20)
  ; 205,275 -> 138,109
  (road city-loc-41 city-loc-20)
  (= (road-length city-loc-41 city-loc-20) 18)
  ; 138,109 -> 205,275
  (road city-loc-20 city-loc-41)
  (= (road-length city-loc-20 city-loc-41) 18)
  ; 205,275 -> 347,149
  (road city-loc-41 city-loc-28)
  (= (road-length city-loc-41 city-loc-28) 19)
  ; 347,149 -> 205,275
  (road city-loc-28 city-loc-41)
  (= (road-length city-loc-28 city-loc-41) 19)
  ; 205,275 -> 377,283
  (road city-loc-41 city-loc-31)
  (= (road-length city-loc-41 city-loc-31) 18)
  ; 377,283 -> 205,275
  (road city-loc-31 city-loc-41)
  (= (road-length city-loc-31 city-loc-41) 18)
  ; 612,304 -> 748,385
  (road city-loc-42 city-loc-3)
  (= (road-length city-loc-42 city-loc-3) 16)
  ; 748,385 -> 612,304
  (road city-loc-3 city-loc-42)
  (= (road-length city-loc-3 city-loc-42) 16)
  ; 612,304 -> 456,221
  (road city-loc-42 city-loc-6)
  (= (road-length city-loc-42 city-loc-6) 18)
  ; 456,221 -> 612,304
  (road city-loc-6 city-loc-42)
  (= (road-length city-loc-6 city-loc-42) 18)
  ; 612,304 -> 521,375
  (road city-loc-42 city-loc-29)
  (= (road-length city-loc-42 city-loc-29) 12)
  ; 521,375 -> 612,304
  (road city-loc-29 city-loc-42)
  (= (road-length city-loc-29 city-loc-42) 12)
  ; 612,304 -> 720,241
  (road city-loc-42 city-loc-30)
  (= (road-length city-loc-42 city-loc-30) 13)
  ; 720,241 -> 612,304
  (road city-loc-30 city-loc-42)
  (= (road-length city-loc-30 city-loc-42) 13)
  ; 660,909 -> 564,783
  (road city-loc-43 city-loc-8)
  (= (road-length city-loc-43 city-loc-8) 16)
  ; 564,783 -> 660,909
  (road city-loc-8 city-loc-43)
  (= (road-length city-loc-8 city-loc-43) 16)
  ; 660,909 -> 803,858
  (road city-loc-43 city-loc-14)
  (= (road-length city-loc-43 city-loc-14) 16)
  ; 803,858 -> 660,909
  (road city-loc-14 city-loc-43)
  (= (road-length city-loc-14 city-loc-43) 16)
  ; 660,909 -> 560,901
  (road city-loc-43 city-loc-35)
  (= (road-length city-loc-43 city-loc-35) 10)
  ; 560,901 -> 660,909
  (road city-loc-35 city-loc-43)
  (= (road-length city-loc-35 city-loc-43) 10)
  ; 660,909 -> 463,927
  (road city-loc-43 city-loc-39)
  (= (road-length city-loc-43 city-loc-39) 20)
  ; 463,927 -> 660,909
  (road city-loc-39 city-loc-43)
  (= (road-length city-loc-39 city-loc-43) 20)
  ; 966,112 -> 930,259
  (road city-loc-44 city-loc-12)
  (= (road-length city-loc-44 city-loc-12) 16)
  ; 930,259 -> 966,112
  (road city-loc-12 city-loc-44)
  (= (road-length city-loc-12 city-loc-44) 16)
  ; 966,112 -> 806,18
  (road city-loc-44 city-loc-19)
  (= (road-length city-loc-44 city-loc-19) 19)
  ; 806,18 -> 966,112
  (road city-loc-19 city-loc-44)
  (= (road-length city-loc-19 city-loc-44) 19)
  ; 966,112 -> 858,139
  (road city-loc-44 city-loc-33)
  (= (road-length city-loc-44 city-loc-33) 12)
  ; 858,139 -> 966,112
  (road city-loc-33 city-loc-44)
  (= (road-length city-loc-33 city-loc-44) 12)
  ; 599,133 -> 456,221
  (road city-loc-45 city-loc-6)
  (= (road-length city-loc-45 city-loc-6) 17)
  ; 456,221 -> 599,133
  (road city-loc-6 city-loc-45)
  (= (road-length city-loc-6 city-loc-45) 17)
  ; 599,133 -> 682,8
  (road city-loc-45 city-loc-23)
  (= (road-length city-loc-45 city-loc-23) 15)
  ; 682,8 -> 599,133
  (road city-loc-23 city-loc-45)
  (= (road-length city-loc-23 city-loc-45) 15)
  ; 599,133 -> 720,241
  (road city-loc-45 city-loc-30)
  (= (road-length city-loc-45 city-loc-30) 17)
  ; 720,241 -> 599,133
  (road city-loc-30 city-loc-45)
  (= (road-length city-loc-30 city-loc-45) 17)
  ; 599,133 -> 612,304
  (road city-loc-45 city-loc-42)
  (= (road-length city-loc-45 city-loc-42) 18)
  ; 612,304 -> 599,133
  (road city-loc-42 city-loc-45)
  (= (road-length city-loc-42 city-loc-45) 18)
  (at package-1 city-loc-25)
  (at package-2 city-loc-20)
  (at package-3 city-loc-28)
  (at package-4 city-loc-10)
  (at package-5 city-loc-22)
  (at package-6 city-loc-22)
  (at package-7 city-loc-30)
  (at package-8 city-loc-6)
  (at package-9 city-loc-40)
  (at package-10 city-loc-1)
  (at package-11 city-loc-15)
  (at package-12 city-loc-22)
  (at package-13 city-loc-42)
  (at package-14 city-loc-44)
  (at package-15 city-loc-3)
  (at package-16 city-loc-39)
  (at package-17 city-loc-17)
  (at package-18 city-loc-26)
  (at truck-1 city-loc-6)
  (capacity truck-1 capacity-4)
  (at truck-2 city-loc-20)
  (capacity truck-2 capacity-3)
  (at truck-3 city-loc-1)
  (capacity truck-3 capacity-4)
  (at truck-4 city-loc-23)
  (capacity truck-4 capacity-4)
 )
 (:goal (and
  (at package-1 city-loc-17)
  (at package-2 city-loc-1)
  (at package-3 city-loc-16)
  (at package-4 city-loc-3)
  (at package-5 city-loc-10)
  (at package-6 city-loc-12)
  (at package-7 city-loc-42)
  (at package-8 city-loc-39)
  (at package-9 city-loc-29)
  (at package-10 city-loc-36)
  (at package-11 city-loc-21)
  (at package-12 city-loc-25)
  (at package-13 city-loc-20)
  (at package-14 city-loc-45)
  (at package-15 city-loc-30)
  (at package-16 city-loc-24)
  (at package-17 city-loc-3)
  (at package-18 city-loc-3)
 ))
 (:metric minimize (total-cost))
)
