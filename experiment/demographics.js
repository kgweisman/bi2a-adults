/* set up demographics slide */

// set up button behavior
$('.slide#demographics button').click(function() { 

	console.log('click')
	// record demographic info...
	// text inputs
	experiment.allData.age = $('input#age', '#demographicsForm').val();

	// text areas
	experiment.allData.comments = $('.slide#demographics textarea#comments').val();

	// multiple choice radios
	experiment.allData.gender = $('input[name=gender]:checked', '#demographicsForm').val();
	experiment.allData.education = $('input[name=education]:checked', '#demographicsForm').val();
	experiment.allData.englishNative = $('input[name=englishNative]:checked', '#demographicsForm').val();

	// multiple answer checkboxes
	$('input[name=ethnicity]:checked', '#demographicsForm').each(function() {
		experiment.allData.ethnicity.push($(this).val());
	});
	$('input[name=religion]:checked', '#demographicsForm').each(function() {
		experiment.allData.religion.push($(this).val());
	});

	// end session
	opener.turk.submit(experiment);
	window.scrollTo(0, 0);
	showSlide("finished");
});
