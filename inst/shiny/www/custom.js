$("document").ready(function() {
    setTimeout(function() {
        $("body").toggleClass('sidebar-collapse', false);
        $("li.treeview").children("a").first().click();
    },5);
});

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
