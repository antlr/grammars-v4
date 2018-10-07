;"Author:     Rob Myers <rob@robmyers.org> 2013"
;"License:    http://creativecommons.org/publicdomain/zero/1.0/"
;"-----------------------------------------------------------------------------"

@create $feature called "Cupcake Feature"

@describe "Cupcake Feature" as "A feature object for giving cupcakes to players.."
@set "Cupcake Feature".help_msg to {"A feature object for giving cupcakes to players.", "Use cupc*ake <player> to give a cupcake to someone.", "Use cupc*ake everyone to give cupcakes to everyone in the room.
"}

@property "Cupcake Feature".bases {"angel cake", "angel food cake", "carrot cake", "cheesecake", "chiffon cake", "chocolate cake", "coffee cake", "cucumber cake", "layer cake", "marble cake", "muffin", "pound cake", "pumpkin bread", "red velvet cake", "spice cake", "sponge cake", "wedding cake"} rc

@property "Cupcake feature".icings {"icing", "butter icing", "buttercream icing", "chocolate icing", "cream cheese icing", "fondant", "sour cream icing", "ganache", "marzipan"} rc

@property "Cupcake Feature".decorations {"a birthday candle stuck in it", "a cherry on top", "a gumball on top", "a sparkler on top", "chocolate sprinkles", "edible ball-bearings", "mini marshmallows", "mini chocolate buttons", "rainbow sprinkles", "sprinkles"} rc

@property "Cupcake Feature".decoration_probability 3 rc

@verb "Cupcake Feature":describe_cupcake tnt rxd
@program "Cupcake Feature":describe_cupcake
cupcake_desc = this.bases[random($)] + " with " + this.icings[random($)];
if(random(this.decoration_probability) == 1)
    cupcake_desc = cupcake_desc + " and " + this.decorations[random($)];
endif
return cupcake_desc;
.

@verb "Cupcake Feature":cupc*ake any any any rxd
@program "Cupcake Feature":cupcake
if (!dobjstr)
  msg = "%N %<eats> a cupcake. Oooh! It's " + this:describe_cupcake() + ".";
elseif (dobjstr == "everyone")
  msg = "%N %<gives> everyone cupcakes. Oooh! They're " + this:describe_cupcake() + ".";
else
  dobj = $string_utils:match_player(dobjstr);
  if (dobj == $failed_match || dobj == $ambiguous_match)
    player:tell("They're not here.");
    return;
  endif
  msg = "%N %<gives> %d a cupcake. Oooh! It's " + this:describe_cupcake() + ".";
endif
$you:say_action(msg);
.
