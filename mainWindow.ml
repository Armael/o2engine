
type boutonsChoix = {positionBoutons: int * int; valeurs: string array; sel: int; hidden : bool ;action: unit -> unit}

let setAction {positionBoutons = p; valeurs = v; sel = s; hidden = b; action = a} ns =
	 {positionBoutons = p; valeurs = v; sel = s; hidden = b ; action = ns}

let doButton p v s b a = {positionBoutons = p; valeurs = v; sel = s; hidden = b ; action = a}

let drawButton objArray =
	Graphics.set_color (Graphics.rgb 232 232 232);
	Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
	for i = 0 to (Array.length objArray) - 1 do
		let obj = objArray.(i) in
			let x = fst (obj.positionBoutons) and y = snd (obj.positionBoutons) in
				if not (obj.hidden) then
				begin
					let nombre = (Array.length obj.valeurs) in
						for i = 0 to nombre - 1 do
							Graphics.set_color Graphics.black;
							Graphics.moveto (x + 16) (y - 1 + (nombre - 1 - i) * 16);
							Graphics.draw_string (obj.valeurs.(i));
							Graphics.fill_circle (x + 6) (y + 6 + (nombre - 1 - i) * 16) 6;
							if obj.sel = i then
							begin
								Graphics.set_color (Graphics.rgb 17 133 244);
								Graphics.fill_circle (x + 6) (y + 6 + (nombre - 1 - i) * 16) 5;
								Graphics.set_color Graphics.black;
								Graphics.fill_circle (x + 6) (y + 6 + (nombre - 1 - i) * 16) 3;
							end
							else
							begin
								Graphics.set_color Graphics.white;
								Graphics.fill_circle (x + 6) (y + 6 + (nombre - 1 - i) * 16) 5;
							end;
						done;
				end;
	done

let evenements objArray =
	let fin = ref (true) in
		Graphics.set_color (Graphics.rgb 232 232 232);
		Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
		drawButton objArray;
		Graphics.set_color Graphics.foreground;
		while !fin do
			let status = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Button_down] in
				if (status.Graphics.keypressed) then fin := false
				else
					let x = status.Graphics.mouse_x and y = status.Graphics.mouse_y in
						for i = 0 to (Array.length objArray) - 1 do
							let objet = objArray.(i) in
								let xo = fst (objet.positionBoutons) and yo = snd (objet.positionBoutons) in
									let nombre = (Array.length (objet.valeurs)) in
										for j = 0 to nombre - 1 do
											if ((x >= (xo) && x <= (xo + 12)) && (y >= (yo + (nombre - 1 - j) * 16) && y <= (yo + 12 + (nombre - 1 - j) * 16))) then
											begin
												{objet with sel = j}.action ();
												objArray.(i) <- {objet with sel = j};
												drawButton objArray;
											end;
										done;
						done;
		done

let openWindow () =
	Graphics.open_graph " 200x100";
		let boutons1 = doButton (5, 5) [|"billard";"angry birds";"test 1";"test 2"|] 0 false (fun () -> ()) in
				let objets = [|boutons1|] in
				evenements objets;
				Graphics.close_graph();
				objets.(0).sel