
$(function () {
    'use strict';

    // Initialize ajax autocomplete:
    $('#autocomplete-ajax').autocomplete({
        serviceUrl: '/api/suggest/',
        lookupFilter: function(suggestion, originalQuery, queryLowerCase) {
            var re = new RegExp('^' + $.Autocomplete.utils.escapeRegExChars(queryLowerCase), 'i');     
            return re.test(suggestion.value);
        },
        onSelect: function(suggestion) {
            window.open('http://www.erlang.org/doc/man/' + suggestion.data, 'erlang');
//            $('#selction-ajax').html('You selected: ' + suggestion.value + ', ' + suggestion.data);
        },
        onHint: function (hint) {
            $('#autocomplete-ajax-x').val(hint);
        },
        onInvalidateSelection: function() {
            $('#selction-ajax').html('You selected: none');
        }
    });
    
});