/**
 * Created by pglah on 01.12.2017.
 */


ocpu.seturl("https://amun-software.ocpu.io/Processingservice/R");
//ocpu.seturl("//cloud.opencpu.org/ocpu/library/Processingservice/R");



var mysession;

$("#submitbutton").on("click", function() {

    console.log($("#pngfile")[0].files[0]);
    console.log($("#pngfile")[0].files[1]);

    var myfile1 = $("#pngfile")[0].files[0];
    var myfile2 = $("#pngfile")[0].files[1];
    var ndvi = ocpu.call("NDVI_Result", {
            x: myfile1,
            y: myfile2

    },
    function (session) {
        mysession = session;

       var key = $("#key").text(mysession.getKey());
      var location = $("#location").text(mysession.getLoc());

        //retrieve session console (async)
        mysession.getConsole(function(outtxt){
            $("#output").text(outtxt);
        });

        //enable the other button
        $("#getbutton").removeAttr("disabled");

        var graphics = $("#plotdiv");
        var graphicsURL = mysession.getLoc() + "graphics/last/";
        graphics.prepend('<img id="theImg" src='+ graphicsURL + ' />');
        console.log(graphicsURL);


    })
        ndvi.fail(function(){
        alert("Error: " + req.responseText);
    });
});

$("#getbutton").click(function(){



    mysession.getObject(function(data) {
        $("#plotdiv").append(data);
    });
});
