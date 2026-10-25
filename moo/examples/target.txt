@program Drone:targets_in_room
targets = {};
for candidate in (player.location.contents)
  if (parent(candidate) == this.target_parent)
       targets = listappend(targets, candidate);
  endif
endfor
return targets;
.
