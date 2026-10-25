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
