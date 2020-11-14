;"Author:     Rob Myers <rob@robmyers.org> 2013"
;"License:    http://creativecommons.org/publicdomain/zero/1.0/"
;"-----------------------------------------------------------------------------"

@create $feature called "Drone"

;@describe "Drone" as "A feature object for drone strikes."
;@notedit "Drone".help_msg
;delete
;enter
;Provides the following commands:
;strike => Hit any pseudo guests in the room, moving them somewhere appropriate.
;.
;save
;done

@property Drone.target_parent #56953 rc
@property Drone.destinations {#78071} rc
@property Drone.messages {"An unexpected drone strike hits %targets."} rc

@verb Drone:destination tnt rxd
@program Drone:destination
return this.destinations[random($)];
.

@verb Drone:message tnt rxd
@program Drone:message
return this.messages[random($)];
.

@verb Drone:targets_in_room tnt rxd
@program Drone:targets_in_room
targets = {};
for candidate in (player.location.contents)
  if (parent(candidate) == this.target_parent)
       targets = listappend(targets, candidate);
  endif
endfor
return targets;
.

@verb Drone:strike any any any rxd
@program Drone:strike
targets = this:targets_in_room();
if (targets)
  place = this:destination();
  names = {};
  for target in (targets)
    names = listappend(names, target.name);
    target:moveto(place);
  endfor
  substitutions = {{"%targets", $string_utils:english_list(names)}};
  announcement = $string_utils:substitute(this:message(), substitutions);
  player.location:announce_all(announcement);
endif;
.

;$string_utils:match_object("Drone", me):set_feature_verbs({"strike"})
