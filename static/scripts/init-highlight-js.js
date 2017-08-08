$(document).ready(function () {
    hljs.initHighlighting();
    $(".declaration").each(function (i, block) {
        hljs.highlightBlock(block);
    });
});
