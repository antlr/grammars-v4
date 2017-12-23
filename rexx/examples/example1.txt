/* GUESSING GAME -- try guessing the randomly-generated number */

do while end_this_game <> 'N'

   the_number = random(1,5)  /* generate the random number */

   say "I’m thinking of a number between 1 and 5, what is it?"

   pull your_guess
   say "You entered: " your_guess

   if your_guess = the_number then
     say 'You guessed it! The number was: ' the_number
   else do
     say 'Sorry, the number was: ' the_number
     say 'We should have bet on this!'
   end

   say 'Try again? [Y or N]: ' 
   pull end_this_game
end

