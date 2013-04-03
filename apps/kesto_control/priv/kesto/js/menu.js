$(document).ready(function () {

	function checkSessionId() {
		var sessionId = $.cookie("session_id");
		
		
		return false;
	}
	
	function initialize() {
		if(!checkSessionId()){
			$('#sign_in').lightbox_me({
				centered: true, 
				onLoad: function() { 
					$('#sign_in').find('input:first').focus()
				}
			});
		}
	}

	initialize();

	// Subscribe to the 'templateSwitch' event.
	// This function will run when a template is switched.
	$.kestoControl.sub('templateSwitch', function (templateName) {
		if (templateName === 'menu') {
			pingAllowed = true;
			initialize();
		}
	});
});
