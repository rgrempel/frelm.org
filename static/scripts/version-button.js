$(document).ready(function () {
    var elmVersion = localStorage.getItem("elm-version") || "ev-container-any";
    $(document.body).attr("class", elmVersion);

    $(".version-button[data-set-version=\"" + elmVersion + "\"]")
        .removeClass("btn-default")
        .addClass("btn-primary");

    $(".version-button").click(function () {
        var version = $(this).attr("data-set-version");
        localStorage.setItem("elm-version", version);
        $(document.body).attr("class", version);

        $(".version-button")
            .removeClass("btn-primary")
            .addClass("btn-default");
        $(".version-button[data-set-version=\"" + version + "\"]")
            .removeClass("btn-default")
            .addClass("btn-primary");
    });
});
