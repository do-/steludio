	my ($data) = @_;

	return

		draw_table (

			[
				'Наименование',
			],

			sub{

				draw_cells ({
					href => "/?type=__TYPE__&id=$$i{id}",
				},[
	
					$i -> {label},

					{
						type => "checkbox",
						name => "___TYPE___$$i{id}",
					},

				])

			},

			$data -> {__TYPE__},

			{
				title => {label => ...},

				top_toolbar=>[{
						keep_params => ['type', 'select'],
					},
					{
						icon  => 'create',
						label => '&Добавить',
						href  => '?type=__TYPE__&action=create',
					},

					{
						type  => 'input_text',
						label => 'Искать',
						name  => 'q',
						keep_params => [],
					},

					{
						type    => 'pager',
						cnt     => 0 + @{$data -> {__TYPE__}},
						total   => $data -> {cnt},
						portion => $data -> {portion},
					},

					fake_select (),

				],
				
				toolbar => draw_centered_toolbar ({},[
					{
						icon    => 'ok',
						label   => 'слить выделенные',
						href    => 'javaScript:document.form.submit()',
						confirm => 'Вы уверены, что?..',
					},
				]),
			}
		);
