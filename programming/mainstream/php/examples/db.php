<?
/*
Copyright 2000-2001 KMR Enterprises
Scripted by TDavid @ http://www.tdscripts.com/ 
php-scripts.com Example #27
Purpose: to add and remove users from a mySQL table

This code is made available freely to modify as long as
this COMPLETE header is not removed. If you decide to use this 
code then please put up a reciprocal link back to our site at
http://www.tdscripts.com/

We cannot, and will not, be held liable for any use or misuse
of the code contained herein. Any upload or execution of the
code implies understanding and agreement of these terms of use.
*/

$db = "DATABASE NAME";
$admin = "MYSQL USER NAME";
$adpass = "MYSQL PASSWORD";
$mysql_link = mysql_connect("localhost", $admin, $adpass);
mysql_select_db($db, $mysql_link);

?>
<HTML>
<BODY>
<?
if($react == "delete_user") {
   if($user) {
      $query = "DELETE from login WHERE user='$user' ";
      $result = mysql_query($query, $mysql_link);
         if(mysql_num_rows($result)) {
            print("<strong>$user</strong> successfully deleted<p>");
         }
   } else { 
      print("<strong>no users are available to delete yet, sorry.</strong><p>");
   }
} 
elseif ($react == "add_user") {
   if(($user) and ($pass)) {
      $query = "INSERT into login VALUES ( ";
      $query .= "0, SYSDATE(), '$username', '$password' )";
      mysql_query($query, $mysql_link);
   } else {
      print("<strong>either your user or password field was left blank</strong><p>");
   }
}
else { 
   print("<center>Administration Area - Choose your option</center>"); 
}?>
<form method="POST" action="admin.php3"><div align="center"><center><p>Delete Users 
<input type="hidden" name="react" value="delete_user">
<select name="user" size="1">
<?
   $query = "SELECT user FROM login ORDER BY user";
   $result = mysql_query($query, $mysql_link);
     if(mysql_num_rows($result)) {
       // we have at least one user, so show all users as options in select form
       while($row = mysql_fetch_row($result))
       {
          print("<option value=\"$row[0]\">$row[0]</option>");
       }
     } else {
       print("<option value=\"\">No users created yet</option>");
     }
?>
</select><input type="submit" value="Submit"></p></center></div>
</form>
<form method="POST" action="admin.php3">
<input type="hidden" name="react" value="add_user">
<div align="center"><center><p>ADD A New User<br>
User: <input type="text" name="user" size="20"><br>
Pass: <input type="text" name="pass" size="20"><br>
<input type="submit" value="Submit"></p></center></div>
</form>
</BODY>
</HTML>