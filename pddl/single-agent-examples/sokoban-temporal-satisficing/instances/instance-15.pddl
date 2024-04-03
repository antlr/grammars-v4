;; ###################
;; #.   .#.   .#.   .#
;; # $ $ # $ $ # $ $ #
;; #.# #.#.# #.#.# #.#
;; # $ $ # $ $ # $ $ #
;; ### #####@##### ###
;;   #   @*@*@*@   #
;;   ###############

(define (problem p009-multiban-temporal)
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
    pos-01-01 - location
    pos-01-02 - location
    pos-01-03 - location
    pos-01-04 - location
    pos-01-05 - location
    pos-01-06 - location
    pos-01-07 - location
    pos-01-08 - location
    pos-02-01 - location
    pos-02-02 - location
    pos-02-03 - location
    pos-02-04 - location
    pos-02-05 - location
    pos-02-06 - location
    pos-02-07 - location
    pos-02-08 - location
    pos-03-01 - location
    pos-03-02 - location
    pos-03-03 - location
    pos-03-04 - location
    pos-03-05 - location
    pos-03-06 - location
    pos-03-07 - location
    pos-03-08 - location
    pos-04-01 - location
    pos-04-02 - location
    pos-04-03 - location
    pos-04-04 - location
    pos-04-05 - location
    pos-04-06 - location
    pos-04-07 - location
    pos-04-08 - location
    pos-05-01 - location
    pos-05-02 - location
    pos-05-03 - location
    pos-05-04 - location
    pos-05-05 - location
    pos-05-06 - location
    pos-05-07 - location
    pos-05-08 - location
    pos-06-01 - location
    pos-06-02 - location
    pos-06-03 - location
    pos-06-04 - location
    pos-06-05 - location
    pos-06-06 - location
    pos-06-07 - location
    pos-06-08 - location
    pos-07-01 - location
    pos-07-02 - location
    pos-07-03 - location
    pos-07-04 - location
    pos-07-05 - location
    pos-07-06 - location
    pos-07-07 - location
    pos-07-08 - location
    pos-08-01 - location
    pos-08-02 - location
    pos-08-03 - location
    pos-08-04 - location
    pos-08-05 - location
    pos-08-06 - location
    pos-08-07 - location
    pos-08-08 - location
    pos-09-01 - location
    pos-09-02 - location
    pos-09-03 - location
    pos-09-04 - location
    pos-09-05 - location
    pos-09-06 - location
    pos-09-07 - location
    pos-09-08 - location
    pos-10-01 - location
    pos-10-02 - location
    pos-10-03 - location
    pos-10-04 - location
    pos-10-05 - location
    pos-10-06 - location
    pos-10-07 - location
    pos-10-08 - location
    pos-11-01 - location
    pos-11-02 - location
    pos-11-03 - location
    pos-11-04 - location
    pos-11-05 - location
    pos-11-06 - location
    pos-11-07 - location
    pos-11-08 - location
    pos-12-01 - location
    pos-12-02 - location
    pos-12-03 - location
    pos-12-04 - location
    pos-12-05 - location
    pos-12-06 - location
    pos-12-07 - location
    pos-12-08 - location
    pos-13-01 - location
    pos-13-02 - location
    pos-13-03 - location
    pos-13-04 - location
    pos-13-05 - location
    pos-13-06 - location
    pos-13-07 - location
    pos-13-08 - location
    pos-14-01 - location
    pos-14-02 - location
    pos-14-03 - location
    pos-14-04 - location
    pos-14-05 - location
    pos-14-06 - location
    pos-14-07 - location
    pos-14-08 - location
    pos-15-01 - location
    pos-15-02 - location
    pos-15-03 - location
    pos-15-04 - location
    pos-15-05 - location
    pos-15-06 - location
    pos-15-07 - location
    pos-15-08 - location
    pos-16-01 - location
    pos-16-02 - location
    pos-16-03 - location
    pos-16-04 - location
    pos-16-05 - location
    pos-16-06 - location
    pos-16-07 - location
    pos-16-08 - location
    pos-17-01 - location
    pos-17-02 - location
    pos-17-03 - location
    pos-17-04 - location
    pos-17-05 - location
    pos-17-06 - location
    pos-17-07 - location
    pos-17-08 - location
    pos-18-01 - location
    pos-18-02 - location
    pos-18-03 - location
    pos-18-04 - location
    pos-18-05 - location
    pos-18-06 - location
    pos-18-07 - location
    pos-18-08 - location
    pos-19-01 - location
    pos-19-02 - location
    pos-19-03 - location
    pos-19-04 - location
    pos-19-05 - location
    pos-19-06 - location
    pos-19-07 - location
    pos-19-08 - location
    stone-01 - stone
    stone-02 - stone
    stone-03 - stone
    stone-04 - stone
    stone-05 - stone
    stone-06 - stone
    stone-07 - stone
    stone-08 - stone
    stone-09 - stone
    stone-10 - stone
    stone-11 - stone
    stone-12 - stone
    stone-13 - stone
    stone-14 - stone
    stone-15 - stone
  )
  (:init
    (IS-GOAL pos-02-02)
    (IS-GOAL pos-02-04)
    (IS-GOAL pos-06-02)
    (IS-GOAL pos-06-04)
    (IS-GOAL pos-08-02)
    (IS-GOAL pos-08-04)
    (IS-GOAL pos-08-07)
    (IS-GOAL pos-10-07)
    (IS-GOAL pos-12-02)
    (IS-GOAL pos-12-04)
    (IS-GOAL pos-12-07)
    (IS-GOAL pos-14-02)
    (IS-GOAL pos-14-04)
    (IS-GOAL pos-18-02)
    (IS-GOAL pos-18-04)
    (IS-NONGOAL pos-01-01)
    (IS-NONGOAL pos-01-02)
    (IS-NONGOAL pos-01-03)
    (IS-NONGOAL pos-01-04)
    (IS-NONGOAL pos-01-05)
    (IS-NONGOAL pos-01-06)
    (IS-NONGOAL pos-01-07)
    (IS-NONGOAL pos-01-08)
    (IS-NONGOAL pos-02-01)
    (IS-NONGOAL pos-02-03)
    (IS-NONGOAL pos-02-05)
    (IS-NONGOAL pos-02-06)
    (IS-NONGOAL pos-02-07)
    (IS-NONGOAL pos-02-08)
    (IS-NONGOAL pos-03-01)
    (IS-NONGOAL pos-03-02)
    (IS-NONGOAL pos-03-03)
    (IS-NONGOAL pos-03-04)
    (IS-NONGOAL pos-03-05)
    (IS-NONGOAL pos-03-06)
    (IS-NONGOAL pos-03-07)
    (IS-NONGOAL pos-03-08)
    (IS-NONGOAL pos-04-01)
    (IS-NONGOAL pos-04-02)
    (IS-NONGOAL pos-04-03)
    (IS-NONGOAL pos-04-04)
    (IS-NONGOAL pos-04-05)
    (IS-NONGOAL pos-04-06)
    (IS-NONGOAL pos-04-07)
    (IS-NONGOAL pos-04-08)
    (IS-NONGOAL pos-05-01)
    (IS-NONGOAL pos-05-02)
    (IS-NONGOAL pos-05-03)
    (IS-NONGOAL pos-05-04)
    (IS-NONGOAL pos-05-05)
    (IS-NONGOAL pos-05-06)
    (IS-NONGOAL pos-05-07)
    (IS-NONGOAL pos-05-08)
    (IS-NONGOAL pos-06-01)
    (IS-NONGOAL pos-06-03)
    (IS-NONGOAL pos-06-05)
    (IS-NONGOAL pos-06-06)
    (IS-NONGOAL pos-06-07)
    (IS-NONGOAL pos-06-08)
    (IS-NONGOAL pos-07-01)
    (IS-NONGOAL pos-07-02)
    (IS-NONGOAL pos-07-03)
    (IS-NONGOAL pos-07-04)
    (IS-NONGOAL pos-07-05)
    (IS-NONGOAL pos-07-06)
    (IS-NONGOAL pos-07-07)
    (IS-NONGOAL pos-07-08)
    (IS-NONGOAL pos-08-01)
    (IS-NONGOAL pos-08-03)
    (IS-NONGOAL pos-08-05)
    (IS-NONGOAL pos-08-06)
    (IS-NONGOAL pos-08-08)
    (IS-NONGOAL pos-09-01)
    (IS-NONGOAL pos-09-02)
    (IS-NONGOAL pos-09-03)
    (IS-NONGOAL pos-09-04)
    (IS-NONGOAL pos-09-05)
    (IS-NONGOAL pos-09-06)
    (IS-NONGOAL pos-09-07)
    (IS-NONGOAL pos-09-08)
    (IS-NONGOAL pos-10-01)
    (IS-NONGOAL pos-10-02)
    (IS-NONGOAL pos-10-03)
    (IS-NONGOAL pos-10-04)
    (IS-NONGOAL pos-10-05)
    (IS-NONGOAL pos-10-06)
    (IS-NONGOAL pos-10-08)
    (IS-NONGOAL pos-11-01)
    (IS-NONGOAL pos-11-02)
    (IS-NONGOAL pos-11-03)
    (IS-NONGOAL pos-11-04)
    (IS-NONGOAL pos-11-05)
    (IS-NONGOAL pos-11-06)
    (IS-NONGOAL pos-11-07)
    (IS-NONGOAL pos-11-08)
    (IS-NONGOAL pos-12-01)
    (IS-NONGOAL pos-12-03)
    (IS-NONGOAL pos-12-05)
    (IS-NONGOAL pos-12-06)
    (IS-NONGOAL pos-12-08)
    (IS-NONGOAL pos-13-01)
    (IS-NONGOAL pos-13-02)
    (IS-NONGOAL pos-13-03)
    (IS-NONGOAL pos-13-04)
    (IS-NONGOAL pos-13-05)
    (IS-NONGOAL pos-13-06)
    (IS-NONGOAL pos-13-07)
    (IS-NONGOAL pos-13-08)
    (IS-NONGOAL pos-14-01)
    (IS-NONGOAL pos-14-03)
    (IS-NONGOAL pos-14-05)
    (IS-NONGOAL pos-14-06)
    (IS-NONGOAL pos-14-07)
    (IS-NONGOAL pos-14-08)
    (IS-NONGOAL pos-15-01)
    (IS-NONGOAL pos-15-02)
    (IS-NONGOAL pos-15-03)
    (IS-NONGOAL pos-15-04)
    (IS-NONGOAL pos-15-05)
    (IS-NONGOAL pos-15-06)
    (IS-NONGOAL pos-15-07)
    (IS-NONGOAL pos-15-08)
    (IS-NONGOAL pos-16-01)
    (IS-NONGOAL pos-16-02)
    (IS-NONGOAL pos-16-03)
    (IS-NONGOAL pos-16-04)
    (IS-NONGOAL pos-16-05)
    (IS-NONGOAL pos-16-06)
    (IS-NONGOAL pos-16-07)
    (IS-NONGOAL pos-16-08)
    (IS-NONGOAL pos-17-01)
    (IS-NONGOAL pos-17-02)
    (IS-NONGOAL pos-17-03)
    (IS-NONGOAL pos-17-04)
    (IS-NONGOAL pos-17-05)
    (IS-NONGOAL pos-17-06)
    (IS-NONGOAL pos-17-07)
    (IS-NONGOAL pos-17-08)
    (IS-NONGOAL pos-18-01)
    (IS-NONGOAL pos-18-03)
    (IS-NONGOAL pos-18-05)
    (IS-NONGOAL pos-18-06)
    (IS-NONGOAL pos-18-07)
    (IS-NONGOAL pos-18-08)
    (IS-NONGOAL pos-19-01)
    (IS-NONGOAL pos-19-02)
    (IS-NONGOAL pos-19-03)
    (IS-NONGOAL pos-19-04)
    (IS-NONGOAL pos-19-05)
    (IS-NONGOAL pos-19-06)
    (IS-NONGOAL pos-19-07)
    (IS-NONGOAL pos-19-08)
    (MOVE-DIR pos-01-07 pos-01-08 dir-down)
    (MOVE-DIR pos-01-07 pos-02-07 dir-right)
    (MOVE-DIR pos-01-08 pos-01-07 dir-up)
    (MOVE-DIR pos-01-08 pos-02-08 dir-right)
    (MOVE-DIR pos-02-02 pos-02-03 dir-down)
    (MOVE-DIR pos-02-02 pos-03-02 dir-right)
    (MOVE-DIR pos-02-03 pos-02-02 dir-up)
    (MOVE-DIR pos-02-03 pos-02-04 dir-down)
    (MOVE-DIR pos-02-03 pos-03-03 dir-right)
    (MOVE-DIR pos-02-04 pos-02-03 dir-up)
    (MOVE-DIR pos-02-04 pos-02-05 dir-down)
    (MOVE-DIR pos-02-05 pos-02-04 dir-up)
    (MOVE-DIR pos-02-05 pos-03-05 dir-right)
    (MOVE-DIR pos-02-07 pos-01-07 dir-left)
    (MOVE-DIR pos-02-07 pos-02-08 dir-down)
    (MOVE-DIR pos-02-08 pos-01-08 dir-left)
    (MOVE-DIR pos-02-08 pos-02-07 dir-up)
    (MOVE-DIR pos-03-02 pos-02-02 dir-left)
    (MOVE-DIR pos-03-02 pos-03-03 dir-down)
    (MOVE-DIR pos-03-02 pos-04-02 dir-right)
    (MOVE-DIR pos-03-03 pos-02-03 dir-left)
    (MOVE-DIR pos-03-03 pos-03-02 dir-up)
    (MOVE-DIR pos-03-03 pos-04-03 dir-right)
    (MOVE-DIR pos-03-05 pos-02-05 dir-left)
    (MOVE-DIR pos-03-05 pos-04-05 dir-right)
    (MOVE-DIR pos-04-02 pos-03-02 dir-left)
    (MOVE-DIR pos-04-02 pos-04-03 dir-down)
    (MOVE-DIR pos-04-02 pos-05-02 dir-right)
    (MOVE-DIR pos-04-03 pos-03-03 dir-left)
    (MOVE-DIR pos-04-03 pos-04-02 dir-up)
    (MOVE-DIR pos-04-03 pos-04-04 dir-down)
    (MOVE-DIR pos-04-03 pos-05-03 dir-right)
    (MOVE-DIR pos-04-04 pos-04-03 dir-up)
    (MOVE-DIR pos-04-04 pos-04-05 dir-down)
    (MOVE-DIR pos-04-05 pos-03-05 dir-left)
    (MOVE-DIR pos-04-05 pos-04-04 dir-up)
    (MOVE-DIR pos-04-05 pos-04-06 dir-down)
    (MOVE-DIR pos-04-05 pos-05-05 dir-right)
    (MOVE-DIR pos-04-06 pos-04-05 dir-up)
    (MOVE-DIR pos-04-06 pos-04-07 dir-down)
    (MOVE-DIR pos-04-07 pos-04-06 dir-up)
    (MOVE-DIR pos-04-07 pos-05-07 dir-right)
    (MOVE-DIR pos-05-02 pos-04-02 dir-left)
    (MOVE-DIR pos-05-02 pos-05-03 dir-down)
    (MOVE-DIR pos-05-02 pos-06-02 dir-right)
    (MOVE-DIR pos-05-03 pos-04-03 dir-left)
    (MOVE-DIR pos-05-03 pos-05-02 dir-up)
    (MOVE-DIR pos-05-03 pos-06-03 dir-right)
    (MOVE-DIR pos-05-05 pos-04-05 dir-left)
    (MOVE-DIR pos-05-05 pos-06-05 dir-right)
    (MOVE-DIR pos-05-07 pos-04-07 dir-left)
    (MOVE-DIR pos-05-07 pos-06-07 dir-right)
    (MOVE-DIR pos-06-02 pos-05-02 dir-left)
    (MOVE-DIR pos-06-02 pos-06-03 dir-down)
    (MOVE-DIR pos-06-03 pos-05-03 dir-left)
    (MOVE-DIR pos-06-03 pos-06-02 dir-up)
    (MOVE-DIR pos-06-03 pos-06-04 dir-down)
    (MOVE-DIR pos-06-04 pos-06-03 dir-up)
    (MOVE-DIR pos-06-04 pos-06-05 dir-down)
    (MOVE-DIR pos-06-05 pos-05-05 dir-left)
    (MOVE-DIR pos-06-05 pos-06-04 dir-up)
    (MOVE-DIR pos-06-07 pos-05-07 dir-left)
    (MOVE-DIR pos-06-07 pos-07-07 dir-right)
    (MOVE-DIR pos-07-07 pos-06-07 dir-left)
    (MOVE-DIR pos-07-07 pos-08-07 dir-right)
    (MOVE-DIR pos-08-02 pos-08-03 dir-down)
    (MOVE-DIR pos-08-02 pos-09-02 dir-right)
    (MOVE-DIR pos-08-03 pos-08-02 dir-up)
    (MOVE-DIR pos-08-03 pos-08-04 dir-down)
    (MOVE-DIR pos-08-03 pos-09-03 dir-right)
    (MOVE-DIR pos-08-04 pos-08-03 dir-up)
    (MOVE-DIR pos-08-04 pos-08-05 dir-down)
    (MOVE-DIR pos-08-05 pos-08-04 dir-up)
    (MOVE-DIR pos-08-05 pos-09-05 dir-right)
    (MOVE-DIR pos-08-07 pos-07-07 dir-left)
    (MOVE-DIR pos-08-07 pos-09-07 dir-right)
    (MOVE-DIR pos-09-02 pos-08-02 dir-left)
    (MOVE-DIR pos-09-02 pos-09-03 dir-down)
    (MOVE-DIR pos-09-02 pos-10-02 dir-right)
    (MOVE-DIR pos-09-03 pos-08-03 dir-left)
    (MOVE-DIR pos-09-03 pos-09-02 dir-up)
    (MOVE-DIR pos-09-03 pos-10-03 dir-right)
    (MOVE-DIR pos-09-05 pos-08-05 dir-left)
    (MOVE-DIR pos-09-05 pos-10-05 dir-right)
    (MOVE-DIR pos-09-07 pos-08-07 dir-left)
    (MOVE-DIR pos-09-07 pos-10-07 dir-right)
    (MOVE-DIR pos-10-02 pos-09-02 dir-left)
    (MOVE-DIR pos-10-02 pos-10-03 dir-down)
    (MOVE-DIR pos-10-02 pos-11-02 dir-right)
    (MOVE-DIR pos-10-03 pos-09-03 dir-left)
    (MOVE-DIR pos-10-03 pos-10-02 dir-up)
    (MOVE-DIR pos-10-03 pos-10-04 dir-down)
    (MOVE-DIR pos-10-03 pos-11-03 dir-right)
    (MOVE-DIR pos-10-04 pos-10-03 dir-up)
    (MOVE-DIR pos-10-04 pos-10-05 dir-down)
    (MOVE-DIR pos-10-05 pos-09-05 dir-left)
    (MOVE-DIR pos-10-05 pos-10-04 dir-up)
    (MOVE-DIR pos-10-05 pos-10-06 dir-down)
    (MOVE-DIR pos-10-05 pos-11-05 dir-right)
    (MOVE-DIR pos-10-06 pos-10-05 dir-up)
    (MOVE-DIR pos-10-06 pos-10-07 dir-down)
    (MOVE-DIR pos-10-07 pos-09-07 dir-left)
    (MOVE-DIR pos-10-07 pos-10-06 dir-up)
    (MOVE-DIR pos-10-07 pos-11-07 dir-right)
    (MOVE-DIR pos-11-02 pos-10-02 dir-left)
    (MOVE-DIR pos-11-02 pos-11-03 dir-down)
    (MOVE-DIR pos-11-02 pos-12-02 dir-right)
    (MOVE-DIR pos-11-03 pos-10-03 dir-left)
    (MOVE-DIR pos-11-03 pos-11-02 dir-up)
    (MOVE-DIR pos-11-03 pos-12-03 dir-right)
    (MOVE-DIR pos-11-05 pos-10-05 dir-left)
    (MOVE-DIR pos-11-05 pos-12-05 dir-right)
    (MOVE-DIR pos-11-07 pos-10-07 dir-left)
    (MOVE-DIR pos-11-07 pos-12-07 dir-right)
    (MOVE-DIR pos-12-02 pos-11-02 dir-left)
    (MOVE-DIR pos-12-02 pos-12-03 dir-down)
    (MOVE-DIR pos-12-03 pos-11-03 dir-left)
    (MOVE-DIR pos-12-03 pos-12-02 dir-up)
    (MOVE-DIR pos-12-03 pos-12-04 dir-down)
    (MOVE-DIR pos-12-04 pos-12-03 dir-up)
    (MOVE-DIR pos-12-04 pos-12-05 dir-down)
    (MOVE-DIR pos-12-05 pos-11-05 dir-left)
    (MOVE-DIR pos-12-05 pos-12-04 dir-up)
    (MOVE-DIR pos-12-07 pos-11-07 dir-left)
    (MOVE-DIR pos-12-07 pos-13-07 dir-right)
    (MOVE-DIR pos-13-07 pos-12-07 dir-left)
    (MOVE-DIR pos-13-07 pos-14-07 dir-right)
    (MOVE-DIR pos-14-02 pos-14-03 dir-down)
    (MOVE-DIR pos-14-02 pos-15-02 dir-right)
    (MOVE-DIR pos-14-03 pos-14-02 dir-up)
    (MOVE-DIR pos-14-03 pos-14-04 dir-down)
    (MOVE-DIR pos-14-03 pos-15-03 dir-right)
    (MOVE-DIR pos-14-04 pos-14-03 dir-up)
    (MOVE-DIR pos-14-04 pos-14-05 dir-down)
    (MOVE-DIR pos-14-05 pos-14-04 dir-up)
    (MOVE-DIR pos-14-05 pos-15-05 dir-right)
    (MOVE-DIR pos-14-07 pos-13-07 dir-left)
    (MOVE-DIR pos-14-07 pos-15-07 dir-right)
    (MOVE-DIR pos-15-02 pos-14-02 dir-left)
    (MOVE-DIR pos-15-02 pos-15-03 dir-down)
    (MOVE-DIR pos-15-02 pos-16-02 dir-right)
    (MOVE-DIR pos-15-03 pos-14-03 dir-left)
    (MOVE-DIR pos-15-03 pos-15-02 dir-up)
    (MOVE-DIR pos-15-03 pos-16-03 dir-right)
    (MOVE-DIR pos-15-05 pos-14-05 dir-left)
    (MOVE-DIR pos-15-05 pos-16-05 dir-right)
    (MOVE-DIR pos-15-07 pos-14-07 dir-left)
    (MOVE-DIR pos-15-07 pos-16-07 dir-right)
    (MOVE-DIR pos-16-02 pos-15-02 dir-left)
    (MOVE-DIR pos-16-02 pos-16-03 dir-down)
    (MOVE-DIR pos-16-02 pos-17-02 dir-right)
    (MOVE-DIR pos-16-03 pos-15-03 dir-left)
    (MOVE-DIR pos-16-03 pos-16-02 dir-up)
    (MOVE-DIR pos-16-03 pos-16-04 dir-down)
    (MOVE-DIR pos-16-03 pos-17-03 dir-right)
    (MOVE-DIR pos-16-04 pos-16-03 dir-up)
    (MOVE-DIR pos-16-04 pos-16-05 dir-down)
    (MOVE-DIR pos-16-05 pos-15-05 dir-left)
    (MOVE-DIR pos-16-05 pos-16-04 dir-up)
    (MOVE-DIR pos-16-05 pos-16-06 dir-down)
    (MOVE-DIR pos-16-05 pos-17-05 dir-right)
    (MOVE-DIR pos-16-06 pos-16-05 dir-up)
    (MOVE-DIR pos-16-06 pos-16-07 dir-down)
    (MOVE-DIR pos-16-07 pos-15-07 dir-left)
    (MOVE-DIR pos-16-07 pos-16-06 dir-up)
    (MOVE-DIR pos-17-02 pos-16-02 dir-left)
    (MOVE-DIR pos-17-02 pos-17-03 dir-down)
    (MOVE-DIR pos-17-02 pos-18-02 dir-right)
    (MOVE-DIR pos-17-03 pos-16-03 dir-left)
    (MOVE-DIR pos-17-03 pos-17-02 dir-up)
    (MOVE-DIR pos-17-03 pos-18-03 dir-right)
    (MOVE-DIR pos-17-05 pos-16-05 dir-left)
    (MOVE-DIR pos-17-05 pos-18-05 dir-right)
    (MOVE-DIR pos-18-02 pos-17-02 dir-left)
    (MOVE-DIR pos-18-02 pos-18-03 dir-down)
    (MOVE-DIR pos-18-03 pos-17-03 dir-left)
    (MOVE-DIR pos-18-03 pos-18-02 dir-up)
    (MOVE-DIR pos-18-03 pos-18-04 dir-down)
    (MOVE-DIR pos-18-04 pos-18-03 dir-up)
    (MOVE-DIR pos-18-04 pos-18-05 dir-down)
    (MOVE-DIR pos-18-05 pos-17-05 dir-left)
    (MOVE-DIR pos-18-05 pos-18-04 dir-up)
    (MOVE-DIR pos-18-07 pos-18-08 dir-down)
    (MOVE-DIR pos-18-07 pos-19-07 dir-right)
    (MOVE-DIR pos-18-08 pos-18-07 dir-up)
    (MOVE-DIR pos-18-08 pos-19-08 dir-right)
    (MOVE-DIR pos-19-07 pos-18-07 dir-left)
    (MOVE-DIR pos-19-07 pos-19-08 dir-down)
    (MOVE-DIR pos-19-08 pos-18-08 dir-left)
    (MOVE-DIR pos-19-08 pos-19-07 dir-up)
    (at player-01 pos-10-06)
    (at player-02 pos-07-07)
    (at player-03 pos-09-07)
    (at player-04 pos-11-07)
    (at player-05 pos-13-07)
    (at stone-01 pos-03-03)
    (at stone-02 pos-05-03)
    (at stone-03 pos-09-03)
    (at stone-04 pos-11-03)
    (at stone-05 pos-15-03)
    (at stone-06 pos-17-03)
    (at stone-07 pos-03-05)
    (at stone-08 pos-05-05)
    (at stone-09 pos-09-05)
    (at stone-10 pos-11-05)
    (at stone-11 pos-15-05)
    (at stone-12 pos-17-05)
    (at stone-13 pos-08-07)
    (at stone-14 pos-10-07)
    (at stone-15 pos-12-07)
    (at-goal stone-13)
    (at-goal stone-14)
    (at-goal stone-15)
    (clear pos-01-07)
    (clear pos-01-08)
    (clear pos-02-02)
    (clear pos-02-03)
    (clear pos-02-04)
    (clear pos-02-05)
    (clear pos-02-07)
    (clear pos-02-08)
    (clear pos-03-02)
    (clear pos-04-02)
    (clear pos-04-03)
    (clear pos-04-04)
    (clear pos-04-05)
    (clear pos-04-06)
    (clear pos-04-07)
    (clear pos-05-02)
    (clear pos-05-07)
    (clear pos-06-02)
    (clear pos-06-03)
    (clear pos-06-04)
    (clear pos-06-05)
    (clear pos-06-07)
    (clear pos-08-02)
    (clear pos-08-03)
    (clear pos-08-04)
    (clear pos-08-05)
    (clear pos-09-02)
    (clear pos-10-02)
    (clear pos-10-03)
    (clear pos-10-04)
    (clear pos-10-05)
    (clear pos-11-02)
    (clear pos-12-02)
    (clear pos-12-03)
    (clear pos-12-04)
    (clear pos-12-05)
    (clear pos-14-02)
    (clear pos-14-03)
    (clear pos-14-04)
    (clear pos-14-05)
    (clear pos-14-07)
    (clear pos-15-02)
    (clear pos-15-07)
    (clear pos-16-02)
    (clear pos-16-03)
    (clear pos-16-04)
    (clear pos-16-05)
    (clear pos-16-06)
    (clear pos-16-07)
    (clear pos-17-02)
    (clear pos-18-02)
    (clear pos-18-03)
    (clear pos-18-04)
    (clear pos-18-05)
    (clear pos-18-07)
    (clear pos-18-08)
    (clear pos-19-07)
    (clear pos-19-08)
  )
  (:goal (and
    (at-goal stone-01)
    (at-goal stone-02)
    (at-goal stone-03)
    (at-goal stone-04)
    (at-goal stone-05)
    (at-goal stone-06)
    (at-goal stone-07)
    (at-goal stone-08)
    (at-goal stone-09)
    (at-goal stone-10)
    (at-goal stone-11)
    (at-goal stone-12)
    (at-goal stone-13)
    (at-goal stone-14)
    (at-goal stone-15)
  ))
  (:metric minimize (total-time))
)
