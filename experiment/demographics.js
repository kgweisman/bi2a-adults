/* set up demographics slide */

var clickHandler = function(event) {

	// record demographic info...
	experiment.allData.age = $('input#age', '#demographicsForm').val();
	experiment.allData.gender = $('input[name=gender]:checked', '#demographicsForm').val();
	experiment.allData.education = $('input[name=education]:checked', '#demographicsForm').val();
	$('input[name=religion]:checked', '#demographicsForm').each(function() {
		experiment.allData.religion.push($(this).val());
	});
	$('input[name=ethnicity]:checked', '#demographicsForm').each(function() {
		experiment.allData.ethnicity.push($(this).val());
	});
	experiment.allData.englishNative = $('input[name=englishNative]:checked', '#demographicsForm').val();
	experiment.allData.comments = $('.slide#demographics textarea#comments').val();
};

// set up button behavior
$('.slide#demographics button').click(function() {
	
	// record data
	clickHandler();

	// submit to turk (using iframe, i.e., opener) and show finished slide
	// turk.submit(experiment);
	opener.turk.submit(experiment);
	showSlide("finished");
});
