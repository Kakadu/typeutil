open Printf

let _ =
  let module VL = View.List    (View.Integer) in
  let module VA = View.Array   (View.Integer) in
  let module S  = Set.Make     (Compare.Integer) in
  let module VS = View.Set     (S) (View.Integer) in
  let module M  = Map.Make     (Compare.Integer) in
  let module H  = Hashtbl.Make 
      (
       struct 

	 include Compare.Integer 

	 let equal x y = (compare x y) = 0 
	 let hash = Hashtbl.hash 

       end
      ) 
  in
  let module VH = View.Hashtbl (H) (View.Integer) (View.Integer) in
  let module VM = View.Map     (M) (View.Integer) (View.Integer) in
  printf "%s\n" (VL.toString [1; 2; 3; 4; 5]);
  printf "%s\n" (VA.toString [|1; 2; 3; 4; 5|]);
  let s = List.fold_left (fun acc x -> S.add x acc) S.empty [1; 2; 3; 4; 5] in
  printf "%s\n" (VS.toString s);
  let m = List.fold_left (fun acc (x, y) -> M.add x y acc) M.empty [1, 2; 3, 4; 5, 6; 7, 8; 9, 10; 11, 12] in
  printf "%s\n" (VM.toString m);
  let h = H.create 5 in
  List.iter (fun (x, y) -> H.add h x y) [1, 2; 3, 4; 5, 6; 7, 8; 9, 10; 11, 12];
  printf "%s\n" (VH.toString h)

  
  
