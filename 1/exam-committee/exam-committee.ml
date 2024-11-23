type vote = StrongReject | WeakReject | WeakAccept | StrongAccept;;
let accept a = a=WeakAccept || a=StrongAccept;;
let accept_both a b = (accept a) && (accept b);;
let accept_two a b c = (accept_both a b) || (accept_both b c) || (accept_both a b);;
let reject a = a=StrongReject;;
let reject_one a b c = (reject a) || (reject b) || (reject c);;
let exam_committee a b c = (not (reject_one a b c)) && (accept_two a b c);;