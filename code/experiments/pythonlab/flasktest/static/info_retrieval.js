$(document).ready(function() {
	console.log('hi');
	$("#query_button").click(function(event) {
		$.get('/hello', function(data) {
			$("#response").html(data);
		});
		event.stopPropagation();
	});
	

});
