<?php EcHo "asdf"; //comment? > ?>
<?= "qwer"; // echo ?>
<?= "zxcv" // missing semicolon ?>

<html>asdf</html>

<div class="container">
    <?php if (jf::Check("workshop")): ?>
        <li><a href="asdf">Dashboard</a></li>
    <?php endIf ;?>
</div>

<p>
<?php switch($a): case 1: // without semicolon?>
        <br>
    <?php break ?>
    <?php case 2: ?>
        <br>
    <?php break;?>
    <?php case 3: ?>
        <br>
    <?php break;?>
<?php endswitch; ?>

<?php if ($a == 5) { ?>
<sample></sample>
<?php }; ?>