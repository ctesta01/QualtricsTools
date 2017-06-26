// This Javascript file makes heavy use of j-query to select elements of the page
// and apply changes to all selected elements. Javascript is primarily for dynamically updating
// the contents of a web-page. In the #submit button code later, I make use of the fact that
// both JavaScript and Shiny are reacting to updates to user input and since it was easier to make
// the submit button send the data through javascript than to get Shiny to do this I had the
// Javascript compute the data export tags associated to the selected and unselected boxes in the
// include/exclude questions page and send them to Shiny through the javascript method Shiny.onInputChange

// The StackOverflow documentation on the Shiny Javascript-API is the best, I think:
// https://stackoverflow.com/documentation/shiny/3149/javascript-api/19807/sending-data-from-client-to-server#t=201706261946375739113
// I haven't been able to find other good documentation, even from Shiny.

// jQuery is a very powerful library, read more about how to use it at https://jquery.com/
// Everywhere you see a $(...) in this document, it is jQuery.



// Make the sidebar opens automatically
$("document").ready(function() {
  setTimeout(function() {
    $("body").toggleClass('sidebar-collapse', false);
    $("li.treeview").children("a").first().click();
  }, 5);
});

// Make the select-all/unselect-all button select all the checkboxes
// on the page if not all of them are already checked, and to uncheck
// all of them if they are all already checked.
$("document").ready(function() {
  $('#selectAll').click(function(e){
    var table= $("#select_qdict").find(".dataTables_wrapper");
    if ($('td input:checkbox:checked',table).length == $('td input:checkbox',table).length) {
      $('td input:checkbox',table).removeAttr("checked");
    } else {
      $('td input:checkbox',table).prop('checked', true);
    }
  });
});

// Bold things on the display logic page, so that they are more readable
$("document").ready(function() {
  setInterval(function() {
    if ($('li:contains("display logic")').attr("class") == "active") {
      selection = $('td:contains("Choice Display Logic:")');
      selection = selection.add($('td:contains("Question Display Logic:")'));
      selection = selection.add($('td:contains("Skip Logic:")'));
      selection = selection.add($('td:contains("Display Logic for:")'));
      selection = selection.add($('td:contains("Answer Display Logic:")'));
      selection.each(function(index) { this.style.fontWeight = 'bold'; });
    }
  }, 100);
});

// Send the lists of the selected and unselected questions to Shiny when the #submit button is clicked
// through the Shiny.onInputChange method.
$('document').ready(function() {
  $('#submit').click(function(e) {
    var unselected_output = [];
    var unselected = $('td input:checkbox:not(:checked)').parent().siblings(':nth-child(2)').map(function() {
      return($(this).text());
    });
    for (var i=0; i<unselected.length; i++) unselected_output.push(unselected[i]);
    var selected_output = [];
    var selected = $('td input:checkbox:checked').parent().siblings(':nth-child(2)').map(function() {
      return($(this).text());
    });
    for (i=0; i<selected.length; i++) selected_output.push(selected[i]);
    Shiny.onInputChange('unselected_questions', unselected_output);
    Shiny.onInputChange('selected_questions', selected_output);
  });
});

// Add the navbar-fixed-top class to the logo and change the class of the header for styling.
$('document').ready(function() {
  $('.logo').addClass('navbar-fixed-top');
  $('.navbar').removeClass( "navbar-static-top" ).addClass( "navbar-fixed-top" );
});

// Remove the btn-default from the download buttons for styling.
$('document').ready(function() {
  $('.btn-primary').removeClass("btn-default");
});
