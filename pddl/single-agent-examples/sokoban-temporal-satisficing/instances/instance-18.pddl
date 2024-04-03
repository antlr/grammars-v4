;; #######
;; #+#@#@#
;; #.#$#$##
;; #.#@@$@#
;; #.#$####
;; #.$@@#
;; # $@+#
;; ######

(define (problem p007-multiban-temporal)
  (:domain sokoban-temporal)
  (:objects
    dir-down - direction
    dir-left - direction
    dir-right - direction
    dir-up - direction
    player-01 - player
    player-02 - player
    player-03 - player
    player-04 - player
    player-05 - player
    player-06 - player
    player-07 - player
    player-08 - player
    player-09 - player
    player-10 - player
    pos-1-1 - location
    pos-1-2 - location
    pos-1-3 - location
    pos-1-4 - location
    pos-1-5 - location
    pos-1-6 - location
    pos-1-7 - location
    pos-1-8 - location
    pos-2-1 - location
    pos-2-2 - location
    pos-2-3 - location
    pos-2-4 - location
    pos-2-5 - location
    pos-2-6 - location
    pos-2-7 - location
    pos-2-8 - location
    pos-3-1 - location
    pos-3-2 - location
    pos-3-3 - location
    pos-3-4 - location
    pos-3-5 - location
    pos-3-6 - location
    pos-3-7 - location
    pos-3-8 - location
    pos-4-1 - location
    pos-4-2 - location
    pos-4-3 - location
    pos-4-4 - location
    pos-4-5 - location
    pos-4-6 - location
    pos-4-7 - location
    pos-4-8 - location
    pos-5-1 - location
    pos-5-2 - location
    pos-5-3 - location
    pos-5-4 - location
    pos-5-5 - location
    pos-5-6 - location
    pos-5-7 - location
    pos-5-8 - location
    pos-6-1 - location
    pos-6-2 - location
    pos-6-3 - location
    pos-6-4 - location
    pos-6-5 - location
    pos-6-6 - location
    pos-6-7 - location
    pos-6-8 - location
    pos-7-1 - location
    pos-7-2 - location
    pos-7-3 - location
    pos-7-4 - location
    pos-7-5 - location
    pos-7-6 - location
    pos-7-7 - location
    pos-7-8 - location
    pos-8-1 - location
    pos-8-2 - location
    pos-8-3 - location
    pos-8-4 - location
    pos-8-5 - location
    pos-8-6 - location
    pos-8-7 - location
    pos-8-8 - location
    stone-01 - stone
    stone-02 - stone
    stone-03 - stone
    stone-04 - stone
    stone-05 - stone
    stone-06 - stone
  )
  (:init
    (IS-GOAL pos-2-2)
    (IS-GOAL pos-2-3)
    (IS-GOAL pos-2-4)
    (IS-GOAL pos-2-5)
    (IS-GOAL pos-2-6)
    (IS-GOAL pos-5-7)
    (IS-NONGOAL pos-1-1)
    (IS-NONGOAL pos-1-2)
    (IS-NONGOAL pos-1-3)
    (IS-NONGOAL pos-1-4)
    (IS-NONGOAL pos-1-5)
    (IS-NONGOAL pos-1-6)
    (IS-NONGOAL pos-1-7)
    (IS-NONGOAL pos-1-8)
    (IS-NONGOAL pos-2-1)
    (IS-NONGOAL pos-2-7)
    (IS-NONGOAL pos-2-8)
    (IS-NONGOAL pos-3-1)
    (IS-NONGOAL pos-3-2)
    (IS-NONGOAL pos-3-3)
    (IS-NONGOAL pos-3-4)
    (IS-NONGOAL pos-3-5)
    (IS-NONGOAL pos-3-6)
    (IS-NONGOAL pos-3-7)
    (IS-NONGOAL pos-3-8)
    (IS-NONGOAL pos-4-1)
    (IS-NONGOAL pos-4-2)
    (IS-NONGOAL pos-4-3)
    (IS-NONGOAL pos-4-4)
    (IS-NONGOAL pos-4-5)
    (IS-NONGOAL pos-4-6)
    (IS-NONGOAL pos-4-7)
    (IS-NONGOAL pos-4-8)
    (IS-NONGOAL pos-5-1)
    (IS-NONGOAL pos-5-2)
    (IS-NONGOAL pos-5-3)
    (IS-NONGOAL pos-5-4)
    (IS-NONGOAL pos-5-5)
    (IS-NONGOAL pos-5-6)
    (IS-NONGOAL pos-5-8)
    (IS-NONGOAL pos-6-1)
    (IS-NONGOAL pos-6-2)
    (IS-NONGOAL pos-6-3)
    (IS-NONGOAL pos-6-4)
    (IS-NONGOAL pos-6-5)
    (IS-NONGOAL pos-6-6)
    (IS-NONGOAL pos-6-7)
    (IS-NONGOAL pos-6-8)
    (IS-NONGOAL pos-7-1)
    (IS-NONGOAL pos-7-2)
    (IS-NONGOAL pos-7-3)
    (IS-NONGOAL pos-7-4)
    (IS-NONGOAL pos-7-5)
    (IS-NONGOAL pos-7-6)
    (IS-NONGOAL pos-7-7)
    (IS-NONGOAL pos-7-8)
    (IS-NONGOAL pos-8-1)
    (IS-NONGOAL pos-8-2)
    (IS-NONGOAL pos-8-3)
    (IS-NONGOAL pos-8-4)
    (IS-NONGOAL pos-8-5)
    (IS-NONGOAL pos-8-6)
    (IS-NONGOAL pos-8-7)
    (IS-NONGOAL pos-8-8)
    (MOVE-DIR pos-2-2 pos-2-3 dir-down)
    (MOVE-DIR pos-2-3 pos-2-2 dir-up)
    (MOVE-DIR pos-2-3 pos-2-4 dir-down)
    (MOVE-DIR pos-2-4 pos-2-3 dir-up)
    (MOVE-DIR pos-2-4 pos-2-5 dir-down)
    (MOVE-DIR pos-2-5 pos-2-4 dir-up)
    (MOVE-DIR pos-2-5 pos-2-6 dir-down)
    (MOVE-DIR pos-2-6 pos-2-5 dir-up)
    (MOVE-DIR pos-2-6 pos-2-7 dir-down)
    (MOVE-DIR pos-2-6 pos-3-6 dir-right)
    (MOVE-DIR pos-2-7 pos-2-6 dir-up)
    (MOVE-DIR pos-2-7 pos-3-7 dir-right)
    (MOVE-DIR pos-3-6 pos-2-6 dir-left)
    (MOVE-DIR pos-3-6 pos-3-7 dir-down)
    (MOVE-DIR pos-3-6 pos-4-6 dir-right)
    (MOVE-DIR pos-3-7 pos-2-7 dir-left)
    (MOVE-DIR pos-3-7 pos-3-6 dir-up)
    (MOVE-DIR pos-3-7 pos-4-7 dir-right)
    (MOVE-DIR pos-4-2 pos-4-3 dir-down)
    (MOVE-DIR pos-4-3 pos-4-2 dir-up)
    (MOVE-DIR pos-4-3 pos-4-4 dir-down)
    (MOVE-DIR pos-4-4 pos-4-3 dir-up)
    (MOVE-DIR pos-4-4 pos-4-5 dir-down)
    (MOVE-DIR pos-4-4 pos-5-4 dir-right)
    (MOVE-DIR pos-4-5 pos-4-4 dir-up)
    (MOVE-DIR pos-4-5 pos-4-6 dir-down)
    (MOVE-DIR pos-4-6 pos-3-6 dir-left)
    (MOVE-DIR pos-4-6 pos-4-5 dir-up)
    (MOVE-DIR pos-4-6 pos-4-7 dir-down)
    (MOVE-DIR pos-4-6 pos-5-6 dir-right)
    (MOVE-DIR pos-4-7 pos-3-7 dir-left)
    (MOVE-DIR pos-4-7 pos-4-6 dir-up)
    (MOVE-DIR pos-4-7 pos-5-7 dir-right)
    (MOVE-DIR pos-5-4 pos-4-4 dir-left)
    (MOVE-DIR pos-5-4 pos-6-4 dir-right)
    (MOVE-DIR pos-5-6 pos-4-6 dir-left)
    (MOVE-DIR pos-5-6 pos-5-7 dir-down)
    (MOVE-DIR pos-5-7 pos-4-7 dir-left)
    (MOVE-DIR pos-5-7 pos-5-6 dir-up)
    (MOVE-DIR pos-6-2 pos-6-3 dir-down)
    (MOVE-DIR pos-6-3 pos-6-2 dir-up)
    (MOVE-DIR pos-6-3 pos-6-4 dir-down)
    (MOVE-DIR pos-6-4 pos-5-4 dir-left)
    (MOVE-DIR pos-6-4 pos-6-3 dir-up)
    (MOVE-DIR pos-6-4 pos-7-4 dir-right)
    (MOVE-DIR pos-7-4 pos-6-4 dir-left)
    (MOVE-DIR pos-7-6 pos-7-7 dir-down)
    (MOVE-DIR pos-7-6 pos-8-6 dir-right)
    (MOVE-DIR pos-7-7 pos-7-6 dir-up)
    (MOVE-DIR pos-7-7 pos-7-8 dir-down)
    (MOVE-DIR pos-7-7 pos-8-7 dir-right)
    (MOVE-DIR pos-7-8 pos-7-7 dir-up)
    (MOVE-DIR pos-7-8 pos-8-8 dir-right)
    (MOVE-DIR pos-8-1 pos-8-2 dir-down)
    (MOVE-DIR pos-8-2 pos-8-1 dir-up)
    (MOVE-DIR pos-8-6 pos-7-6 dir-left)
    (MOVE-DIR pos-8-6 pos-8-7 dir-down)
    (MOVE-DIR pos-8-7 pos-7-7 dir-left)
    (MOVE-DIR pos-8-7 pos-8-6 dir-up)
    (MOVE-DIR pos-8-7 pos-8-8 dir-down)
    (MOVE-DIR pos-8-8 pos-7-8 dir-left)
    (MOVE-DIR pos-8-8 pos-8-7 dir-up)
    (at player-01 pos-2-2)
    (at player-02 pos-4-2)
    (at player-03 pos-6-2)
    (at player-04 pos-4-4)
    (at player-05 pos-5-4)
    (at player-06 pos-7-4)
    (at player-07 pos-4-6)
    (at player-08 pos-5-6)
    (at player-09 pos-4-7)
    (at player-10 pos-5-7)
    (at stone-01 pos-4-3)
    (at stone-02 pos-6-3)
    (at stone-03 pos-6-4)
    (at stone-04 pos-4-5)
    (at stone-05 pos-3-6)
    (at stone-06 pos-3-7)
    (clear pos-2-3)
    (clear pos-2-4)
    (clear pos-2-5)
    (clear pos-2-6)
    (clear pos-2-7)
    (clear pos-7-6)
    (clear pos-7-7)
    (clear pos-7-8)
    (clear pos-8-1)
    (clear pos-8-2)
    (clear pos-8-6)
    (clear pos-8-7)
    (clear pos-8-8)
  )
  (:goal (and
    (at-goal stone-01)
    (at-goal stone-02)
    (at-goal stone-03)
    (at-goal stone-04)
    (at-goal stone-05)
    (at-goal stone-06)
  ))
  (:metric minimize (total-time))
)
