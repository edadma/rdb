// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	site: 'https://petradb.dev',
	integrations: [
		starlight({
			title: 'PetraDB',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/edadma/petradb' }],
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Introduction', slug: 'getting-started' },
					],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'guides/javascript' },
						{ label: 'Scala', slug: 'guides/scala' },
					],
				},
				{
					label: 'SQL Reference',
					items: [
						{ label: 'SQL Features', slug: 'reference/sql' },
					],
				},
				{
					label: 'API Reference',
					items: [
						{ label: 'API', slug: 'reference/api' },
					],
				},
			],
		}),
	],
});
