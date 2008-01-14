	my ($data) = @_;

	$_REQUEST {__focused_input} = '_label';

	draw_form ({
			right_buttons => [del ($data)],
			
			path => [
				{type => '__TYPE__', name => '???'},
				{type => '__TYPE__', name => $data -> {label}, id => $data -> {id}},
			],
			
		},
		
		$data,
		
		[
			{
				name    => 'label',
				label   => 'Наименование',
				size    => 40,
				max_len => 255,
			},
#			{
#				name   => 'id_user',
#				label  => 'Пользователь',
#				type   => 'select',
#				values => $data -> {users},
#				empty  => '[Выберите пользователя]',
#				other  => '/?type=users',
#			},
		],
	)

	.

	draw_table (

		sub {
		
			draw_cells ({
				bgcolor => $i -> {id} == $data -> {id} ? '#ffffd0' : undef,
			},[
				{
					type => 'checkbox',
					name => "_clone_$$i{id}",
					off  => $i -> {id} == $data -> {id},
				},
				
				$i -> {label},
				
			]),
		
		},
		
		$data -> {clones},
		
		{
			
			title => {label => 'Похожие названия'},
			
			off   => !$_REQUEST{__read_only} || @{$data -> {clones}} < 2,
			
			name  => 't1',
						
			top_toolbar => [{
				keep_params => ['type', 'id'],
			},
				{
					name  => 'first',
					type  => 'input_text',
					label => 'По скольким буквам',
					size  => 2,
					keep_params => [],
				},
			],
			
			toolbar => draw_centered_toolbar ({},
				
				[
					{
						icon  => 'delete',
						label => 'слить выделенные записи с текущей',
						href  => "javaScript:if(confirm('Вы уверены, что все выделенные записи совпадают по смыслу с текущей?')) document.forms ['t1'].submit()",
					}
				]

			),

		}

	);
