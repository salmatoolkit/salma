$(document).ready(function() {
	console.log('hi');
	$("#query_button").click(function(event) {
		$.get('http://localhost:5000/hello', function(data) {
			$("#response").html(data);
		});
		event.stopPropagation();
	});
	

});
