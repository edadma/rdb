// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import react from '@astrojs/react';
import tailwindcss from '@tailwindcss/vite';

// https://astro.build/config
export default defineConfig({
	site: 'https://petradb.dev',
	vite: {
		plugins: [tailwindcss({ optimize: false })],
		ssr: {
			external: [
				'@codemirror/autocomplete', '@codemirror/commands',
				'@codemirror/lang-css', '@codemirror/lang-html',
				'@codemirror/lang-javascript', '@codemirror/lang-json',
				'@codemirror/lang-markdown', '@codemirror/lang-python',
				'@codemirror/lang-sql', '@codemirror/lang-xml',
				'@codemirror/language', '@codemirror/lint',
				'@codemirror/search', '@codemirror/state', '@codemirror/view',
			],
		},
	},
	integrations: [
		react({
			include: ['**/components/**/*.tsx'],
		}),
		starlight({
			title: 'PetraDB',
			social: [
				{ icon: 'github', label: 'GitHub', href: 'https://github.com/edadma/petradb' },
				{ icon: 'npm', label: 'npm', href: 'https://www.npmjs.com/package/@petradb/engine' },
			],
			customCss: ['./src/styles/custom.css'],
			head: [
				{
					tag: 'script',
					content: `(()=>{if(location.pathname.replace(/\\/$/,'').endsWith('/playground'))document.documentElement.classList.add('playground-page')})()`,
				},
			],
			sidebar: [
				{ label: 'Playground', link: '/playground/' },
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
