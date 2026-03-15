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
			defaultLocale: 'root',
			locales: {
				root: { label: 'English', lang: 'en' },
				zh: { label: '简体中文', lang: 'zh-CN' },
				ja: { label: '日本語', lang: 'ja' },
				fr: { label: 'Français', lang: 'fr' },
				es: { label: 'Español', lang: 'es' },
				pt: { label: 'Português', lang: 'pt-BR' },
			},
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
				{
					label: 'Changelog',
					translations: { zh: '更新日志', ja: '変更履歴', fr: 'Journal des modifications', es: 'Registro de cambios', pt: 'Registro de alterações' },
					slug: 'changelog',
				},
				{
					label: 'Getting Started',
					translations: { zh: '快速入门', ja: 'はじめに', fr: 'Démarrage rapide', es: 'Primeros pasos', pt: 'Primeiros passos' },
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'getting-started/javascript' },
						{ label: 'Scala', slug: 'getting-started/scala' },
						{ label: 'Java', slug: 'getting-started/java' },
						{ label: 'C / Native', slug: 'getting-started/c' },
					],
				},
				{
					label: 'Playground',
					translations: { zh: '在线体验', ja: 'プレイグラウンド', fr: 'Terrain de jeu', es: 'Zona de pruebas', pt: 'Playground' },
					link: '/playground/',
				},
				{
					label: 'Guides',
					translations: { zh: '指南', ja: 'ガイド', fr: 'Guides', es: 'Guías', pt: 'Guias' },
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'guides/javascript' },
						{ label: 'Scala', slug: 'guides/scala' },
						{ label: 'CLI', slug: 'guides/cli' },
						{
							label: 'Server',
							translations: { zh: '服务器', ja: 'サーバー', fr: 'Serveur', es: 'Servidor', pt: 'Servidor' },
							slug: 'guides/server',
						},
						{
							label: 'Client',
							translations: { zh: '客户端', ja: 'クライアント', fr: 'Client', es: 'Cliente', pt: 'Cliente' },
							slug: 'guides/client',
						},
					],
				},
				{
					label: 'Integrations',
					translations: { zh: '集成', ja: 'インテグレーション', fr: 'Intégrations', es: 'Integraciones', pt: 'Integrações' },
					items: [
						{ label: 'Drizzle ORM', slug: 'integrations/drizzle' },
						{ label: 'GraphQL', slug: 'integrations/graphql' },
						{ label: 'Knex.js', slug: 'integrations/knex' },
						{ label: 'Lucid ORM', slug: 'integrations/lucid' },
						{ label: 'JDBC', slug: 'integrations/jdbc' },
						{ label: 'Quarry', slug: 'integrations/quarry' },
					],
				},
				{
					label: 'SQL Reference',
					translations: { zh: 'SQL 参考', ja: 'SQL リファレンス', fr: 'Référence SQL', es: 'Referencia SQL', pt: 'Referência SQL' },
					items: [
						{
							label: 'Data Types',
							translations: { zh: '数据类型', ja: 'データ型', fr: 'Types de données', es: 'Tipos de datos', pt: 'Tipos de dados' },
							slug: 'reference/data-types',
						},
						{ label: 'DDL', slug: 'reference/ddl' },
						{ label: 'DML', slug: 'reference/dml' },
						{
							label: 'Queries',
							translations: { zh: '查询', ja: 'クエリ', fr: 'Requêtes', es: 'Consultas', pt: 'Consultas' },
							slug: 'reference/queries',
						},
						{
							label: 'Functions',
							translations: { zh: '函数', ja: '関数', fr: 'Fonctions', es: 'Funciones', pt: 'Funções' },
							slug: 'reference/functions',
						},
						{ label: 'JSON', slug: 'reference/json' },
						{
							label: 'Transactions',
							translations: { zh: '事务', ja: 'トランザクション', fr: 'Transactions', es: 'Transacciones', pt: 'Transações' },
							slug: 'reference/transactions',
						},
						{ label: 'PL/pgSQL', slug: 'reference/plpgsql' },
					],
				},
				{
					label: 'API Reference',
					translations: { zh: 'API 参考', ja: 'API リファレンス', fr: 'Référence API', es: 'Referencia API', pt: 'Referência API' },
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'reference/api-javascript' },
						{ label: 'Scala', slug: 'reference/api-scala' },
						{ label: 'C', slug: 'reference/api-c' },
					],
				},
			],
		}),
	],
});
