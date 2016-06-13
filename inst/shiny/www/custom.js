$("document").ready(function() {
    setTimeout(function() {
        $("body").toggleClass('sidebar-collapse', false);
        $("li.treeview").children("a").first().click();
    },5);
});
