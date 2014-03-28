$(document).ready(function() {
  var qs = window.location.href.split('?')[1];
  if (qs) {
    $("#neighbours").attr("src", "neighbours.html?"+qs);
    $("#routes").attr("src", "routes.html?"+qs);
  }
});
