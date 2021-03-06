// set up helper functions

// define function to show a slide
function showSlide(id) { 
	// console.log(id); // show which slide is being presented
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
};

// define function to get a random integer < n
function randomInteger(n) { 
	return Math.floor(Math.random()*n); 
}

// define function for random selection without replacement
function randomElementNR(bucket) { 
	var randomIndex = randomInteger(bucket.length);
	return bucket.splice(randomIndex, 1)[0];
}

// define function for preloading images (code from long)
function preload(images, onLoadedOne, onLoadedAll) {
	showSlide("preload");

	var remainingImages = images.slice();
	var finished = false;

	// set delayInterval to 800 for testing to see that everything actually loads
	// for real use, set to 0 
	var loadDelayInterval = 0;

	var worker = function() {
	  if (remainingImages.length == 0) {
	    if (!finished) {
	      finished = true;
	      setTimeout(onLoadedAll, loadDelayInterval);
	    }
	} else {

	    var src = remainingImages.shift(); 
	    
	    var image = new Image();
	    image.onload = function() {
	        onLoadedOne();
	        setTimeout(worker, loadDelayInterval);
	    };
	        image.src = src;
		}
	};

	// load images 6 at a time
	var concurrent = 13;
	for(var i = 0; i < concurrent; i++) {
		setTimeout(worker, 20 - i);
		};
	}

// define a function that will get called every time one image is successfully loaded
var numLoadedImages = 0;
function onLoadedOne() {
	numLoadedImages++;
	
	// $("#num-loaded").text(numLoadedImages); 

	// display progress bar
	var percentComplete = (numLoadedImages)/13 * 100;
	var percentCompleteRounded = Math.round(percentComplete);
	$('#preload .progress-bar').attr("aria-valuenow", percentComplete.toString());
	$('#preload .progress-bar').css("width", percentComplete.toString()+"%");
}

// define a function that will get called once all images have been successfully loaded
function onLoadedAll() {
	showSlide("consent");
}