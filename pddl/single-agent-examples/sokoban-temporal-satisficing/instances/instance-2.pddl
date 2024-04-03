;; "svenx008"
;;
;;       # #
;;    # #   #
;;   #     #
;;    # . . #
;;   #   .   #
;;  #   $ $ $ #
;;   # # # *   #
;;      # @ @ #
;;       # # #

(define (problem p008-hexoban-temporal)
  (:domain sokoban-temporal)
  (:objects
    dir-east - direction
    dir-northeast - direction
    dir-northwest - direction
    dir-southeast - direction
    dir-southwest - direction
    dir-west - direction
    player-01 - player
    player-02 - player
    pos-01-01 - location
    pos-01-03 - location
    pos-01-05 - location
    pos-01-07 - location
    pos-01-09 - location
    pos-02-02 - location
    pos-02-04 - location
    pos-02-06 - location
    pos-02-08 - location
    pos-03-01 - location
    pos-03-03 - location
    pos-03-05 - location
    pos-03-07 - location
    pos-03-09 - location
    pos-04-02 - location
    pos-04-04 - location
    pos-04-06 - location
    pos-04-08 - location
    pos-05-01 - location
    pos-05-03 - location
    pos-05-05 - location
    pos-05-07 - location
    pos-05-09 - location
    pos-06-02 - location
    pos-06-04 - location
    pos-06-06 - location
    pos-06-08 - location
    pos-07-01 - location
    pos-07-03 - location
    pos-07-05 - location
    pos-07-07 - location
    pos-07-09 - location
    pos-08-02 - location
    pos-08-04 - location
    pos-08-06 - location
    pos-08-08 - location
    pos-09-01 - location
    pos-09-03 - location
    pos-09-05 - location
    pos-09-07 - location
    pos-09-09 - location
    pos-10-02 - location
    pos-10-04 - location
    pos-10-06 - location
    pos-10-08 - location
    pos-11-01 - location
    pos-11-03 - location
    pos-11-05 - location
    pos-11-07 - location
    pos-11-09 - location
    pos-12-02 - location
    pos-12-04 - location
    pos-12-06 - location
    pos-12-08 - location
    pos-13-01 - location
    pos-13-03 - location
    pos-13-05 - location
    pos-13-07 - location
    pos-13-09 - location
    stone-01 - stone
    stone-02 - stone
    stone-03 - stone
    stone-04 - stone
  )
  (:init
    (IS-GOAL pos-06-04)
    (IS-GOAL pos-07-05)
    (IS-GOAL pos-08-04)
    (IS-GOAL pos-09-07)
    (IS-NONGOAL pos-01-01)
    (IS-NONGOAL pos-01-03)
    (IS-NONGOAL pos-01-05)
    (IS-NONGOAL pos-01-07)
    (IS-NONGOAL pos-01-09)
    (IS-NONGOAL pos-02-02)
    (IS-NONGOAL pos-02-04)
    (IS-NONGOAL pos-02-06)
    (IS-NONGOAL pos-02-08)
    (IS-NONGOAL pos-03-01)
    (IS-NONGOAL pos-03-03)
    (IS-NONGOAL pos-03-05)
    (IS-NONGOAL pos-03-07)
    (IS-NONGOAL pos-03-09)
    (IS-NONGOAL pos-04-02)
    (IS-NONGOAL pos-04-04)
    (IS-NONGOAL pos-04-06)
    (IS-NONGOAL pos-04-08)
    (IS-NONGOAL pos-05-01)
    (IS-NONGOAL pos-05-03)
    (IS-NONGOAL pos-05-05)
    (IS-NONGOAL pos-05-07)
    (IS-NONGOAL pos-05-09)
    (IS-NONGOAL pos-06-02)
    (IS-NONGOAL pos-06-06)
    (IS-NONGOAL pos-06-08)
    (IS-NONGOAL pos-07-01)
    (IS-NONGOAL pos-07-03)
    (IS-NONGOAL pos-07-07)
    (IS-NONGOAL pos-07-09)
    (IS-NONGOAL pos-08-02)
    (IS-NONGOAL pos-08-06)
    (IS-NONGOAL pos-08-08)
    (IS-NONGOAL pos-09-01)
    (IS-NONGOAL pos-09-03)
    (IS-NONGOAL pos-09-05)
    (IS-NONGOAL pos-09-09)
    (IS-NONGOAL pos-10-02)
    (IS-NONGOAL pos-10-04)
    (IS-NONGOAL pos-10-06)
    (IS-NONGOAL pos-10-08)
    (IS-NONGOAL pos-11-01)
    (IS-NONGOAL pos-11-03)
    (IS-NONGOAL pos-11-05)
    (IS-NONGOAL pos-11-07)
    (IS-NONGOAL pos-11-09)
    (IS-NONGOAL pos-12-02)
    (IS-NONGOAL pos-12-04)
    (IS-NONGOAL pos-12-06)
    (IS-NONGOAL pos-12-08)
    (IS-NONGOAL pos-13-01)
    (IS-NONGOAL pos-13-03)
    (IS-NONGOAL pos-13-05)
    (IS-NONGOAL pos-13-07)
    (IS-NONGOAL pos-13-09)
    (MOVE-DIR pos-01-01 pos-02-02 dir-southeast)
    (MOVE-DIR pos-01-01 pos-03-01 dir-east)
    (MOVE-DIR pos-01-03 pos-02-02 dir-northeast)
    (MOVE-DIR pos-01-03 pos-02-04 dir-southeast)
    (MOVE-DIR pos-01-05 pos-02-04 dir-northeast)
    (MOVE-DIR pos-01-07 pos-02-08 dir-southeast)
    (MOVE-DIR pos-01-09 pos-02-08 dir-northeast)
    (MOVE-DIR pos-01-09 pos-03-09 dir-east)
    (MOVE-DIR pos-02-02 pos-01-01 dir-northwest)
    (MOVE-DIR pos-02-02 pos-01-03 dir-southwest)
    (MOVE-DIR pos-02-02 pos-03-01 dir-northeast)
    (MOVE-DIR pos-02-04 pos-01-03 dir-northwest)
    (MOVE-DIR pos-02-04 pos-01-05 dir-southwest)
    (MOVE-DIR pos-02-08 pos-01-07 dir-northwest)
    (MOVE-DIR pos-02-08 pos-01-09 dir-southwest)
    (MOVE-DIR pos-02-08 pos-03-09 dir-southeast)
    (MOVE-DIR pos-02-08 pos-04-08 dir-east)
    (MOVE-DIR pos-03-01 pos-01-01 dir-west)
    (MOVE-DIR pos-03-01 pos-02-02 dir-southwest)
    (MOVE-DIR pos-03-01 pos-05-01 dir-east)
    (MOVE-DIR pos-03-09 pos-01-09 dir-west)
    (MOVE-DIR pos-03-09 pos-02-08 dir-northwest)
    (MOVE-DIR pos-03-09 pos-04-08 dir-northeast)
    (MOVE-DIR pos-03-09 pos-05-09 dir-east)
    (MOVE-DIR pos-04-06 pos-05-05 dir-northeast)
    (MOVE-DIR pos-04-06 pos-06-06 dir-east)
    (MOVE-DIR pos-04-08 pos-02-08 dir-west)
    (MOVE-DIR pos-04-08 pos-03-09 dir-southwest)
    (MOVE-DIR pos-04-08 pos-05-09 dir-southeast)
    (MOVE-DIR pos-05-01 pos-03-01 dir-west)
    (MOVE-DIR pos-05-03 pos-06-04 dir-southeast)
    (MOVE-DIR pos-05-03 pos-07-03 dir-east)
    (MOVE-DIR pos-05-05 pos-04-06 dir-southwest)
    (MOVE-DIR pos-05-05 pos-06-04 dir-northeast)
    (MOVE-DIR pos-05-05 pos-06-06 dir-southeast)
    (MOVE-DIR pos-05-05 pos-07-05 dir-east)
    (MOVE-DIR pos-05-09 pos-03-09 dir-west)
    (MOVE-DIR pos-05-09 pos-04-08 dir-northwest)
    (MOVE-DIR pos-06-04 pos-05-03 dir-northwest)
    (MOVE-DIR pos-06-04 pos-05-05 dir-southwest)
    (MOVE-DIR pos-06-04 pos-07-03 dir-northeast)
    (MOVE-DIR pos-06-04 pos-07-05 dir-southeast)
    (MOVE-DIR pos-06-04 pos-08-04 dir-east)
    (MOVE-DIR pos-06-06 pos-04-06 dir-west)
    (MOVE-DIR pos-06-06 pos-05-05 dir-northwest)
    (MOVE-DIR pos-06-06 pos-07-05 dir-northeast)
    (MOVE-DIR pos-06-06 pos-08-06 dir-east)
    (MOVE-DIR pos-07-03 pos-05-03 dir-west)
    (MOVE-DIR pos-07-03 pos-06-04 dir-southwest)
    (MOVE-DIR pos-07-03 pos-08-02 dir-northeast)
    (MOVE-DIR pos-07-03 pos-08-04 dir-southeast)
    (MOVE-DIR pos-07-05 pos-05-05 dir-west)
    (MOVE-DIR pos-07-05 pos-06-04 dir-northwest)
    (MOVE-DIR pos-07-05 pos-06-06 dir-southwest)
    (MOVE-DIR pos-07-05 pos-08-04 dir-northeast)
    (MOVE-DIR pos-07-05 pos-08-06 dir-southeast)
    (MOVE-DIR pos-07-05 pos-09-05 dir-east)
    (MOVE-DIR pos-08-02 pos-07-03 dir-southwest)
    (MOVE-DIR pos-08-04 pos-06-04 dir-west)
    (MOVE-DIR pos-08-04 pos-07-03 dir-northwest)
    (MOVE-DIR pos-08-04 pos-07-05 dir-southwest)
    (MOVE-DIR pos-08-04 pos-09-05 dir-southeast)
    (MOVE-DIR pos-08-06 pos-06-06 dir-west)
    (MOVE-DIR pos-08-06 pos-07-05 dir-northwest)
    (MOVE-DIR pos-08-06 pos-09-05 dir-northeast)
    (MOVE-DIR pos-08-06 pos-09-07 dir-southeast)
    (MOVE-DIR pos-08-06 pos-10-06 dir-east)
    (MOVE-DIR pos-08-08 pos-09-07 dir-northeast)
    (MOVE-DIR pos-08-08 pos-10-08 dir-east)
    (MOVE-DIR pos-09-05 pos-07-05 dir-west)
    (MOVE-DIR pos-09-05 pos-08-04 dir-northwest)
    (MOVE-DIR pos-09-05 pos-08-06 dir-southwest)
    (MOVE-DIR pos-09-05 pos-10-06 dir-southeast)
    (MOVE-DIR pos-09-07 pos-08-06 dir-northwest)
    (MOVE-DIR pos-09-07 pos-08-08 dir-southwest)
    (MOVE-DIR pos-09-07 pos-10-06 dir-northeast)
    (MOVE-DIR pos-09-07 pos-10-08 dir-southeast)
    (MOVE-DIR pos-09-07 pos-11-07 dir-east)
    (MOVE-DIR pos-10-06 pos-08-06 dir-west)
    (MOVE-DIR pos-10-06 pos-09-05 dir-northwest)
    (MOVE-DIR pos-10-06 pos-09-07 dir-southwest)
    (MOVE-DIR pos-10-06 pos-11-07 dir-southeast)
    (MOVE-DIR pos-10-08 pos-08-08 dir-west)
    (MOVE-DIR pos-10-08 pos-09-07 dir-northwest)
    (MOVE-DIR pos-10-08 pos-11-07 dir-northeast)
    (MOVE-DIR pos-11-01 pos-12-02 dir-southeast)
    (MOVE-DIR pos-11-01 pos-13-01 dir-east)
    (MOVE-DIR pos-11-03 pos-12-02 dir-northeast)
    (MOVE-DIR pos-11-03 pos-12-04 dir-southeast)
    (MOVE-DIR pos-11-03 pos-13-03 dir-east)
    (MOVE-DIR pos-11-07 pos-09-07 dir-west)
    (MOVE-DIR pos-11-07 pos-10-06 dir-northwest)
    (MOVE-DIR pos-11-07 pos-10-08 dir-southwest)
    (MOVE-DIR pos-12-02 pos-11-01 dir-northwest)
    (MOVE-DIR pos-12-02 pos-11-03 dir-southwest)
    (MOVE-DIR pos-12-02 pos-13-01 dir-northeast)
    (MOVE-DIR pos-12-02 pos-13-03 dir-southeast)
    (MOVE-DIR pos-12-04 pos-11-03 dir-northwest)
    (MOVE-DIR pos-12-04 pos-13-03 dir-northeast)
    (MOVE-DIR pos-12-04 pos-13-05 dir-southeast)
    (MOVE-DIR pos-13-01 pos-11-01 dir-west)
    (MOVE-DIR pos-13-01 pos-12-02 dir-southwest)
    (MOVE-DIR pos-13-03 pos-11-03 dir-west)
    (MOVE-DIR pos-13-03 pos-12-02 dir-northwest)
    (MOVE-DIR pos-13-03 pos-12-04 dir-southwest)
    (MOVE-DIR pos-13-05 pos-12-04 dir-northwest)
    (at player-01 pos-08-08)
    (at player-02 pos-10-08)
    (at stone-01 pos-06-06)
    (at stone-02 pos-08-06)
    (at stone-03 pos-10-06)
    (at stone-04 pos-09-07)
    (at-goal stone-04)
    (clear pos-01-01)
    (clear pos-01-03)
    (clear pos-01-05)
    (clear pos-01-07)
    (clear pos-01-09)
    (clear pos-02-02)
    (clear pos-02-04)
    (clear pos-02-08)
    (clear pos-03-01)
    (clear pos-03-09)
    (clear pos-04-06)
    (clear pos-04-08)
    (clear pos-05-01)
    (clear pos-05-03)
    (clear pos-05-05)
    (clear pos-05-09)
    (clear pos-06-04)
    (clear pos-07-03)
    (clear pos-07-05)
    (clear pos-08-02)
    (clear pos-08-04)
    (clear pos-09-05)
    (clear pos-11-01)
    (clear pos-11-03)
    (clear pos-11-07)
    (clear pos-12-02)
    (clear pos-12-04)
    (clear pos-13-01)
    (clear pos-13-03)
    (clear pos-13-05)
    (clear pos-13-09)
  )
  (:goal (and
    (at-goal stone-01)
    (at-goal stone-02)
    (at-goal stone-03)
    (at-goal stone-04)
  ))
  (:metric minimize (total-time))
)
