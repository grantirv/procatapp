$(document).ready(function() {

  // listen for click events on the brinson table cells
   	$("div.gt_brinson").on('click', 'td',
   	  function() {
   	    let ns = $(this).closest('.brinson_container').attr("id")
   	    Shiny.setInputValue(ns + "-clicked_cell",
   	      { row: row_number(this), column: column_number(this) });
    		console.log("clicked cell: " + row_number(this) + ", " + column_number(this));
    	}
  	);

  // add interactive functionality to static brinson gt tables
    $("div.gt_brinson").on('shiny:value',
      function(event) {
        // operate on new table after waiting for it to rendered
          setTimeout(function() {
            let table = $(event.target).find("table");

            // add sortable class to the table
              //table.addClass("sortable");

          }, 100);
      }
    );

});

// edit the table just rendered
  function edit_table() {
    console.log("Just rendered the table");
    alert("Just rendered the table");
  }

// get the row number of the cell indexed from 1
  function row_number(cell) {
  	let i = $(cell).closest('tr').index();
  	return(i + 1);
  }

// get the column number of the cell indexed from 1, excluding row headers
  function column_number(cell){
  	let i = $(cell).index();
  	return(i);
  }
