// make sure the sidebar opens automatically
$("document").ready(function() {
  setTimeout(function() {
    $("body").toggleClass('sidebar-collapse', false);
    $("li.treeview").children("a").first().click();
  }, 5);
});

// this code makes the select-all/unselect-all button do its job
$("document").ready(function() {
  $('#selectAll').click(function(e){
    var table= $("#DataTables_Table_0");

    if ($('td input:checkbox:checked',table).length == $('td input:checkbox',table).length) {
      $('td input:checkbox',table).removeAttr("checked");
    } else {
      $('td input:checkbox',table).prop('checked', true);
    }
  });
});

// bold things on the display logic page, so that they are more readable
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

// send a list of the unselected questions to Shiny when the #submit button is clicked
$('document').ready(function() {
  $('#submit').click(function(e) {
    var unselected = $('td input:checkbox:not(:checked)').parent().siblings(':nth-child(2)').map(function() {
      return($(this).text());
    });
    Shiny.onInputChange('unselected_questions', unselected);
  });
});

// add the navbar-fixed-top class to the logo and change the class of the header
$('document').ready(function() {
  $('.logo').addClass('navbar-fixed-top');
  $('.navbar').removeClass( "navbar-static-top" ).addClass( "navbar-fixed-top" );
});

// remove the btn-default from the download buttons
$('document').ready(function() {
  $('.btn-primary').removeClass("btn-default");
});
