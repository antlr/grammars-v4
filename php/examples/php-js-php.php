<html>
<script src="source.js"></script>
<script type="text/javascript">
    var common_query = '<?php echo PMA_escapeJsString(PMA_generate_common_url('', '', '&'));?>';
    var hints = <?php echo "['No hints']" ?>;
    var url = 'import_status.php?id=<?php echo $upload_id ; ?>';

    function getFrames() {
<?php if ($GLOBALS['text_dir'] === 'ltr') { ?>
        frame_content = window.frames[1];
        frame_navigation = window.frames[0];
<?php } else { ?>
        frame_content = window.frames[0];
        frame_navigation = window.frames[1];
<?php } ?>
    }
</script>
</html>
