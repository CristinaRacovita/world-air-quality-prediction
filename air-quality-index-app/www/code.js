$( document ).ready(function() {
  $( ".navbar .container-fluid" ).append( '<img src="logo.png" class="image"/>' );
});

document.addEventListener("DOMContentLoaded", function() {
  document.getElementById("pm25_aqi_category").setAttribute("readonly", "true");
  document.getElementById("ozone_aqi_category").setAttribute("readonly", "true");
});
