<?
// base class with member properties and methods
class Vegetable {

   var $edible;
   var $color;

   const CONSTANT = 'constant value';

   function Vegetable($edible, $color="green") 
   {
       $this->edible = $edible;
       $this->color = $color;
   }

   function is_edible() 
   {
       return $this->edible;
   }

   function what_color() 
   {
       return $this->color;
   }


} // end of class Vegetable
?>

