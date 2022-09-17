;"Author:     Rob Myers <rob@robmyers.org> 2012"
;"License:    http://creativecommons.org/publicdomain/zero/1.0/"
;"-----------------------------------------------------------------------------"

@verb me:sp*oof any any any
@program me:spoof
"People can still see who called the verb if they know how to look.";
if (this == player)
  player.location:announce_all(argstr);
else
  player:tell("I don't understand that.");
endif
.

@verb me:lick this none none
@program me:lick
"Give him a lick...";
"Many thanks to Dreyannah";
if (this.location != player.location)
  lickfail = "You must be in the same location as %t to lick %[dpo].";
  player:tell($string_utils:pronoun_sub(lickfail));
elseif (this == player)
  lick = "You lick yourself. Mmm! You taste just like raisins.";
  player:tell($string_utils:pronoun_sub(lick));
  licks = "%N %<licks> %r. Mmm! %[dpsc] %<d:tastes> just like raisins.";
  player.location:announce_all_but({player}, $string_utils:pronoun_sub(licks));
else
  lick = "You lick %t. Mmm! %[dpsc] %<d:tastes> just like raisins.";
  player:tell($string_utils:pronoun_sub(lick));
  licks = "%N %<licks> %t. Mmm! %[dpsc] %<d:tastes> just like raisins.";
  but = {this, player};
  player.location:announce_all_but(but, $string_utils:pronoun_sub(licks));
  licked = "%N %<licks> you. Mmm! You taste just like raisins.";
  this:tell($string_utils:pronoun_sub(licked));
endif
.

@verb me:wav*icle any none none
@program me:wavicle
"Greet people midway between a wave and a particle.";
if (this == player)
  if (dobjstr)
    who = $string_utils:match_player(dobjstr);
    if (valid(who) && who in connected_players())
      if(who.location == player.location)
        player:tell($string_utils:pronoun_sub("You wavicle to %d."));
        exclude = {who, this};
        announcement = $string_utils:pronoun_sub("%N wavicles to %d.");
        player.location:announce_all_but(exclude, announcement);
        who:tell($string_utils:pronoun_sub("%N wavicles to you."));
      else
        player:tell($string_utils:pronoun_sub("You wavicle to %d.", dobj=who));
        who:tell($string_utils:pronoun_sub("From %l, %N wavicles to you."));
      endif
    else
      player:tell($string_utils:pronoun_sub("%d is not connected.", dobj=who));
    endif
  else
    player:tell("You wavicle.");
    player.location:announce($string_utils:pronoun_sub("%N wavicles."));
  endif
else
  player:tell("I don't understand that.");
endif
.

