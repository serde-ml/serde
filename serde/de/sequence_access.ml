type ('value, 'error) t = {
  next_element :
    'element_value.
    deser_element:(unit -> ('element_value, 'error Error.de_error) result) ->
    ('element_value option, 'error Error.de_error) result;
}

let next_element t ~deser_element = t.next_element ~deser_element
