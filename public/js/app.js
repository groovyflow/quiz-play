function app() {
//TODO  Use precompiled templates: http://javascriptissexy.com/handlebars-js-tutorial-learn-everything-about-handlebars-js-javascript-templating/
 //and http://handlebarsjs.com/precompilation.html.

    var drawFunc = makeDrawQuestionFunc(
        Handlebars.compile($('#questions_template').html()),
        $('#questionsDiv')
    )

    console.log("Calling getNext with no data yet");
    var nextQuestionPromise = getNext();
    nextQuestionPromise.done(function(data){handleData(data, drawFunc)});
    nextQuestionPromise.fail(function(jqXHR, textStatus, errorThrown){
       alert("Ajax call failed " + errorThrown);
    });

}

function handleData(data, drawFunc) {
    if(data.status === 'continue')  {
        console.log('status is continue')
        resetQuestionNumberHelper();
        drawFunc(data.data);
        listen(data.data, drawFunc);
    }else if(data.status === 'done'){
        resultsDrawFunc = makeDrawQuestionFunc(Handlebars.compile($('#results_template').html()), $('#questionsDiv'));
        resultsDrawFunc(data.result);
    }else
        throw Error("unhandeled status " + data.status)
}

function resetQuestionNumberHelper() {
    //TODO  Does re-regsisterin cause memorey problems?
    var optionIdIndex = 0;
    Handlebars.registerHelper("questionNumber", function () {
        return optionIdIndex++;
    })
}

function makeDrawQuestionFunc(compiledTemplateFunc, selector) {
    return function(data) {
         selector.html(compiledTemplateFunc(data))
    }
}

function listen(currData, drawFunc) {
    //TODO  Is there a memory problem as a result of adding the same listener over and over again?
    $('#questionsForm').submit(function (event) {
        console.log("submit called")
        var answer = $('#questionsForm input[name=questionChoice]:checked').val();
        console.log("the answer is " + answer);
        if (isEmpty(answer)) {
            $('#questionsForm .error').text("Please choose an answer before submitting").show().fadeOut(3000);
        }
        else  {
            var promise = getNext(currData, answer);
            promise.done(function(data){handleData(data, drawFunc)});
            promise.fail(function(jqXHR, textStatus, errorThrown){
                alert("Ajax call failed " + errorThrown);
            });
        }
        event.preventDefault();
    });

}

function getNext(prevQuestion, answer) {
	/*function isInt(value) { 
	    return !isNaN(parseInt(value,10)) && (parseFloat(value,10) == parseInt(value,10)); 
	}
	console.log("answer an int? " + isInt(answer))*/
    return $.ajax({
        contentType: 'application/json',
        data: JSON.stringify({
            prevQuestion: prevQuestion || "NONE", answer: answer || ""
        })/*Without stringify we get a weird error*/,
        dataType: 'json',
        processData: true,
        type: isEmpty(prevQuestion) ? "GET" : "POST",  /*Want to send request body on reply, and can't do that on GET. And after all, a user's quiz reply should change the user's  representation on the server side */
        url: isEmpty(prevQuestion) ? 'quiz/first' : 'quiz/reply'
    });

}


var hasOwnProperty = Object.prototype.hasOwnProperty;

/*From  http://stackoverflow.com/questions/4994201/is-object-empty*/
function isEmpty(obj) {

    // null and undefined are "empty"
    if (obj == null) return true;

    // Assume if it has a length property with a non-zero value
    // that that property is correct.
    if (obj.length > 0)    return false;
    if (obj.length === 0)  return true;

    // Otherwise, does it have any properties of its own?
    // Note that this doesn't handle
    // toString and valueOf enumeration bugs in IE < 9
    for (var key in obj) {
        if (hasOwnProperty.call(obj, key)) return false;
    }

    return true;
}
