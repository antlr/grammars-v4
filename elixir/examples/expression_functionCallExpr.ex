is_function(add, 2)

(fn -> x = 0 end).()

length [1, 2, 3]

hd(list)

put_in users[:john].age, 31

update_in users[:mary].languages, fn languages -> List.delete(languages, "Clojure") end

inspect("hello", binaries: :as_binaries)