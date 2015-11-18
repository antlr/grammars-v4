<script type="text/javascript">
document.addEvent('domready', function() {
	var timers = { timer: <?=$timer?> };

	var TimeTic = function () {
		this.timer--;
		if(this.timer>=0) {
			var m = Math.floor(this.timer/60);
			var s = this.timer - Math.floor(this.timer/60)*60;
			if(s<10 && m<10) {
				s = '0'+s;
			}
			$('login_wait_timer').set('html', m+':'+s);
		} else {
			$clear(timer);
		}
  	}
	var timer = TimeTic.periodical(1000, timers);

    functionOne(<?php echo implode(', ', $arrayWithVars); ?>);
});
</script>
