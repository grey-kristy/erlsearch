
$(function () {
    'use strict';

    // Initialize ajax autocomplete:
    $('#autocomplete-ajax').autocomplete({
        serviceUrl: '/api/suggest/',
        maxHeight: 600,
        lookupFilter: function(suggestion, originalQuery, queryLowerCase) {
            var re = new RegExp('^' + $.Autocomplete.utils.escapeRegExChars(queryLowerCase), 'i');     
            return re.test(suggestion.value);
        },
        onSelect: function(suggestion) {
            var erl_url = 'http://www.erlang.org/doc/man/';
            $('#result').html( $("<a/>", {html: suggestion.value, href: erl_url + suggestion.data}) );
            $('#autocomplete-ajax').val('');
            window.open(erl_url + suggestion.data, 'erlang');
        },
        onHint: function (hint) {
            $('#autocomplete-ajax-x').val(hint);
        },
        onInvalidateSelection: function() {
//            $('#result').html('Nothing is found');
        }
    });
    
});