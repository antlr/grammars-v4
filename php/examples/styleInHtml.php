<html>
	<head>
        <script type="text/javascript" src="<?=WDCPREFIX?>/scripts/swfobject.js"></script> <? // comment ?>
		<style>
			*{
				margin: 0;
				padding: 0;
				}
			.top-banner{
				display:table-cell;
				width: 300px;
				height: 90px;
				vertical-align:middle;
				}
			*+html .middled{
				display:block;
				height: auto;
				width: auto;
				margin-top: expression((parentNode.offsetHeight - this.offsetHeight)<0 ? "0" : (parentNode.offsetHeight - this.offsetHeight)/2 + "px");
			}
			*html .middled{
				display:block;
				height: auto;
				width: auto;
				margin-top: expression((parentNode.offsetHeight - this.offsetHeight)<0 ? "0" : (parentNode.offsetHeight - this.offsetHeight)/2 + "px");
			}
			.top-banner img{
				float:left;
				border: 0px;
				}
		</style>
    </head>
</html>
	