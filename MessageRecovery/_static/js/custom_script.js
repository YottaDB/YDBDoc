/*
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################
*/

/* Custom JavaScript code to handle URL redirection when selecting
   an item from the RELEASES dropdown menu */

document.addEventListener('DOMContentLoaded', function () {
    // Set up the RELEASES dropdown
    if (document.getElementById('select-releases')) {
      setupReleaseSelector()
    };

})

/* Following function is fired as an event when the dropdown element
   is changed.
   The function gets the current URL, splits it and assigns the window
   the new URL (created by concatenating various split strings and
   selected release).
   If a page does not exist in the selected release, the user is directed
   to the index.html page of that manual.
*/
function setupReleaseSelector () {
    document.querySelectorAll('#select-releases').forEach(function (ele) {
	ele.onchange = async function () {
	    var currentURL = window.location.href;
	    var currentURLSplit = currentURL.split('/');
	    var path = currentURLSplit[currentURLSplit.length - 1];
	    var remaining = currentURL.includes(".net") ? "//docs.yottadb.net/" : "//docs.yottadb.com/";
            if (this.value) {
		var currentSel = this.value.replace(".","");
	        var newURL = remaining + currentSel + path;
		const urlStatus = await fetch(newURL);
		if (urlStatus.status === 404) {
		    newURL = remaining + currentSel + "index.html";
	        }
		window.location.assign(newURL);
		document.getElementById(currentSel.split('/')[0]).selected = "true";
	    }
        }
    })
};
