return function(params, receive)
  return function(result, ...)
    if type(result) == "table" and result.status == nil
      and (params.operation == "open" or params.operation == "sync") then
      result.status = { document = params.document .. ":status", revision = 0, block = {} }
    end
    receive(result, ...)
  end
end
